# mod_dataset.R — Panel de gestión de datasets
# RVisual — Addin RStudio para usuarios de SPSS
# Permite cargar archivos (CSV, Excel, RDS, SAV) y detectar data.frames en memoria

library(shiny)
library(miniUI)

# ── UI ────────────────────────────────────────────────────────────────────────

mod_dataset_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Estilos inline del panel
    tags$style(HTML(paste0("
      #", ns("panel_carga"), " { margin-bottom: 12px; }
      .ds-badge { font-size: 11px; padding: 2px 7px; border-radius: 10px;
                  background: #e8f4fd; color: #1a6fa8; font-weight: 600; }
      .ds-row-active { background-color: #f0f7ff !important; font-weight: 600; }
      .ds-hint { font-size: 12px; color: #888; margin-top: 4px; }
    "))),

    fluidRow(
      # ── Columna izquierda: Carga de archivos ──────────────────────────────
      column(5,
        div(id = ns("panel_carga"),
          h5(icon("folder-open"), " Cargar archivo", style = "margin-top:0"),
          fileInput(ns("archivo"),
            label    = NULL,
            accept   = c(".csv", ".xlsx", ".xls", ".rds", ".sav"),
            buttonLabel = "Seleccionar…",
            placeholder = "CSV, Excel, RDS, SAV"
          ),
          # Opciones CSV (se muestran solo si el archivo es CSV)
          conditionalPanel(
            condition = paste0("output['", ns("es_csv"), "']"),
            wellPanel(style = "padding:8px; background:#f8f9fa;",
              fluidRow(
                column(6, selectInput(ns("csv_sep"),   "Separador",
                  choices = c("Coma (,)" = ",", "Punto y coma (;)" = ";",
                              "Tab" = "\t", "Espacio" = " "),
                  selected = ",")),
                column(6, selectInput(ns("csv_enc"),   "Encoding",
                  choices = c("UTF-8" = "UTF-8", "Latin-1" = "latin1"),
                  selected = "UTF-8"))
              ),
              checkboxInput(ns("csv_header"), "Primera fila = encabezado", value = TRUE)
            )
          ),
          # Nombre del dataset
          textInput(ns("nombre_ds"), "Nombre del dataset", placeholder = "ej: datos_encuesta"),
          actionButton(ns("btn_cargar"), "Cargar", icon = icon("upload"),
                       class = "btn-primary btn-sm"),
          div(class = "ds-hint", textOutput(ns("msg_carga")))
        ),

        hr(),

        # ── Data.frames detectados en el entorno global ──────────────────────
        h5(icon("database"), " Entorno global"),
        div(class = "ds-hint", "Data.frames disponibles en memoria:"),
        uiOutput(ns("lista_entorno")),
        actionButton(ns("btn_refresh"), icon("sync"), label = " Actualizar",
                     class = "btn-sm btn-outline-secondary", style = "margin-top:6px")
      ),

      # ── Columna derecha: Datasets cargados ────────────────────────────────
      column(7,
        h5(icon("table"), " Datasets activos"),
        div(style = "min-height: 100px;",
          uiOutput(ns("lista_datasets"))
        ),
        hr(),
        # Info del dataset seleccionado
        uiOutput(ns("info_dataset"))
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_dataset_server <- function(id, rv_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Estado reactivo local
    rv <- reactiveValues(
      datasets    = list(),   # lista nombrada de data.frames cargados
      seleccionado = NULL     # nombre del dataset activo
    )

    # ── ¿Es CSV? (para mostrar opciones) ────────────────────────────────────
    output$es_csv <- reactive({
      req(input$archivo)
      tolower(tools::file_ext(input$archivo$name)) == "csv"
    })
    outputOptions(output, "es_csv", suspendWhenHidden = FALSE)

    # ── Cargar archivo ───────────────────────────────────────────────────────
    observeEvent(input$btn_cargar, {
      req(input$archivo)

      archivo  <- input$archivo
      ext      <- tolower(tools::file_ext(archivo$name))
      nombre   <- trimws(input$nombre_ds)
      if (nchar(nombre) == 0) nombre <- tools::file_path_sans_ext(archivo$name)
      nombre   <- make.names(nombre)   # nombre R válido

      df <- tryCatch({
        switch(ext,
          csv  = read.csv(archivo$datapath,
                          sep    = input$csv_sep,
                          header = input$csv_header,
                          fileEncoding = input$csv_enc,
                          stringsAsFactors = FALSE),
          xlsx = ,
          xls  = readxl::read_excel(archivo$datapath),
          rds  = readRDS(archivo$datapath),
          sav  = haven::read_sav(archivo$datapath),
          stop("Formato no soportado: ", ext)
        )
      }, error = function(e) { showNotification(paste("Error al cargar:", e$message),
                                                 type = "error"); NULL })

      if (!is.null(df) && is.data.frame(df)) {
        rv$datasets[[nombre]] <- as.data.frame(df)
        rv$seleccionado <- nombre
        # Propagar al estado global si existe
        if (!is.null(rv_global)) {
          rv_global$datasets[[nombre]]   <- rv$datasets[[nombre]]
          rv_global$ds_activo            <- nombre
        }
        output$msg_carga <- renderText(paste0("✔ '", nombre, "' cargado (",
                                               nrow(df), " filas × ", ncol(df), " cols)"))
      }
    })

    # ── Detectar data.frames en el entorno global ────────────────────────────
    dfs_entorno <- reactive({
      input$btn_refresh  # dependencia reactiva para refrescar
      objs <- ls(envir = .GlobalEnv)
      objs[sapply(objs, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
    })

    output$lista_entorno <- renderUI({
      dfs <- dfs_entorno()
      if (length(dfs) == 0) {
        return(div(class = "ds-hint", "No hay data.frames en el entorno."))
      }
      tagList(lapply(dfs, function(nm) {
        df_tmp <- get(nm, envir = .GlobalEnv)
        div(style = "margin-bottom:4px;",
          actionLink(ns(paste0("env_", nm)), nm,
                     style = "font-size:13px; text-decoration:none;"),
          span(class = "ds-badge", paste0(nrow(df_tmp), "×", ncol(df_tmp)))
        )
      }))
    })

    # Click en dataset del entorno → importar al panel
    observe({
      dfs <- dfs_entorno()
      lapply(dfs, function(nm) {
        observeEvent(input[[paste0("env_", nm)]], {
          rv$datasets[[nm]]  <- get(nm, envir = .GlobalEnv)
          rv$seleccionado    <- nm
          if (!is.null(rv_global)) {
            rv_global$datasets[[nm]] <- rv$datasets[[nm]]
            rv_global$ds_activo      <- nm
          }
        }, ignoreInit = TRUE)
      })
    })

    # ── Lista de datasets cargados ───────────────────────────────────────────
    output$lista_datasets <- renderUI({
      if (length(rv$datasets) == 0) {
        return(div(class = "ds-hint",
                   icon("info-circle"), " Ningún dataset cargado aún."))
      }
      tagList(lapply(names(rv$datasets), function(nm) {
        df_tmp   <- rv$datasets[[nm]]
        es_activo <- isTRUE(rv$seleccionado == nm)
        div(
          class = if (es_activo) "ds-row-active" else "",
          style = "display:flex; align-items:center; gap:8px;
                   padding:6px 8px; border-radius:6px; margin-bottom:4px;
                   border: 1px solid #dee2e6; cursor:pointer;",
          onclick = paste0("Shiny.setInputValue('", ns("click_ds"), "', '", nm,
                           "', {priority:'event'})"),
          icon(if (es_activo) "check-circle" else "circle", class = "text-primary"),
          span(nm, style = "flex:1; font-size:13px;"),
          span(class = "ds-badge", paste0(nrow(df_tmp), "×", ncol(df_tmp))),
          actionButton(ns(paste0("rm_", nm)), label = NULL, icon = icon("trash"),
                       class = "btn-sm btn-outline-danger",
                       style = "padding:1px 5px; font-size:11px;",
                       onclick = paste0("event.stopPropagation();"))
        )
      }))
    })

    # Seleccionar dataset activo al hacer click
    observeEvent(input$click_ds, {
      nm <- input$click_ds
      if (nm %in% names(rv$datasets)) {
        rv$seleccionado <- nm
        if (!is.null(rv_global)) rv_global$ds_activo <- nm
      }
    })

    # Eliminar dataset
    observe({
      lapply(names(rv$datasets), function(nm) {
        observeEvent(input[[paste0("rm_", nm)]], {
          rv$datasets[[nm]] <- NULL
          if (!is.null(rv_global)) rv_global$datasets[[nm]] <- NULL
          if (isTRUE(rv$seleccionado == nm)) {
            restantes <- names(rv$datasets)
            rv$seleccionado <- if (length(restantes) > 0) restantes[1] else NULL
            if (!is.null(rv_global)) rv_global$ds_activo <- rv$seleccionado
          }
        }, ignoreInit = TRUE)
      })
    })

    # ── Info del dataset seleccionado ────────────────────────────────────────
    output$info_dataset <- renderUI({
      req(rv$seleccionado)
      nm  <- rv$seleccionado
      df  <- rv$datasets[[nm]]
      req(is.data.frame(df))

      tipos <- sapply(df, function(col) {
        cls <- class(col)[1]
        switch(cls,
          numeric   = "num", integer = "int", character = "chr",
          factor    = "fct", logical = "lgl", Date = "date",
          POSIXct   = "dttm", "otro")
      })

      tagList(
        h6(icon("info-circle"), paste0(" ", nm),
           style = "margin-bottom:6px; font-weight:700;"),
        fluidRow(
          column(4, div(class = "ds-badge", paste(nrow(df), "filas"))),
          column(4, div(class = "ds-badge", paste(ncol(df), "columnas"))),
          column(4, div(class = "ds-badge",
                        paste(sum(sapply(df, function(x) any(is.na(x)))), "cols con NA")))
        ),
        tags$hr(style = "margin: 8px 0;"),
        tags$small(
          tags$b("Variables: "),
          paste(paste0(names(tipos), " (", tipos, ")"), collapse = " · ")
        )
      )
    })

    # ── Retornar dataset activo (para que otros módulos lo consuman) ──────────
    return(reactive({
      req(rv$seleccionado)
      rv$datasets[[rv$seleccionado]]
    }))
  })
}
