# mod_dataset.R — Panel de gestión de datasets
# RVisual — Addin RStudio para usuarios de SPSS

# ── UI ────────────────────────────────────────────────────────────────────────

mod_dataset_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0("
      #", ns("panel_carga"), " { margin-bottom: 12px; }
      .ds-badge { font-size: 11px; padding: 2px 7px; border-radius: 10px;
                  background: #e8f4fd; color: #1a6fa8; font-weight: 600; }
      .ds-row-active { background-color: #f0f7ff !important; font-weight: 600; }
      .ds-hint { font-size: 12px; color: #888; margin-top: 4px; }
    "))),

    shiny::fluidRow(
      # ── Columna izquierda: Carga de archivos ──────────────────────────────
      shiny::column(5,
        shiny::div(id = ns("panel_carga"),
          shiny::h5(shiny::icon("folder-open"), " Cargar archivo", style = "margin-top:0"),
          shiny::fileInput(ns("archivo"),
            label       = NULL,
            accept      = c(".csv", ".xlsx", ".xls", ".rds", ".sav"),
            buttonLabel = "Seleccionar\u2026",
            placeholder = "CSV, Excel, RDS, SAV"
          ),
          shiny::conditionalPanel(
            condition = paste0("output['", ns("es_csv"), "']"),
            shiny::wellPanel(style = "padding:8px; background:#f8f9fa;",
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("csv_sep"), "Separador",
                  choices  = c("Coma (,)" = ",", "Punto y coma (;)" = ";",
                               "Tab" = "\t", "Espacio" = " "),
                  selected = ",")),
                shiny::column(6, shiny::selectInput(ns("csv_enc"), "Encoding",
                  choices  = c("UTF-8" = "UTF-8", "Latin-1" = "latin1"),
                  selected = "UTF-8"))
              ),
              shiny::checkboxInput(ns("csv_header"), "Primera fila = encabezado", value = TRUE)
            )
          ),
          shiny::textInput(ns("nombre_ds"), "Nombre del dataset",
                           placeholder = "ej: datos_encuesta"),
          shiny::actionButton(ns("btn_cargar"), "Cargar", icon = shiny::icon("upload"),
                              class = "btn-primary btn-sm"),
          shiny::div(class = "ds-hint", shiny::textOutput(ns("msg_carga")))
        ),

        shiny::hr(),

        shiny::h5(shiny::icon("database"), " Entorno global"),
        shiny::div(class = "ds-hint", "Data.frames disponibles en memoria:"),
        shiny::uiOutput(ns("lista_entorno")),
        shiny::actionButton(ns("btn_refresh"), shiny::icon("sync"),
                            label  = " Actualizar",
                            class  = "btn-sm btn-outline-secondary",
                            style  = "margin-top:6px")
      ),

      # ── Columna derecha: Datasets cargados ────────────────────────────────
      shiny::column(7,
        shiny::h5(shiny::icon("table"), " Datasets activos"),
        shiny::div(style = "min-height: 100px;",
          shiny::uiOutput(ns("lista_datasets"))
        ),
        shiny::hr(),
        shiny::uiOutput(ns("info_dataset"))
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
# Firma alineada con server.R:
#   active_dataset  — reactiveVal(NULL)  para el data.frame activo
#   active_name     — reactiveVal(NULL)  para el nombre del dataset activo
#   history         — reactiveVal(list())para el historial de sesión

mod_dataset_server <- function(id, active_dataset, active_name, history) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Estado local: lista de todos los datasets cargados en este panel
    rv <- shiny::reactiveValues(
      datasets     = list(),
      seleccionado = NULL
    )

    # ── ¿Es CSV? ─────────────────────────────────────────────────────────────
    output$es_csv <- shiny::reactive({
      shiny::req(input$archivo)
      tolower(tools::file_ext(input$archivo$name)) == "csv"
    })
    shiny::outputOptions(output, "es_csv", suspendWhenHidden = FALSE)

    # ── Cargar archivo ────────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_cargar, {
      shiny::req(input$archivo)

      arch   <- input$archivo
      ext    <- tolower(tools::file_ext(arch$name))
      nombre <- trimws(input$nombre_ds)
      if (nchar(nombre) == 0) nombre <- tools::file_path_sans_ext(arch$name)
      nombre <- make.names(nombre)

      df <- tryCatch({
        switch(ext,
          csv  = read.csv(arch$datapath,
                          sep          = input$csv_sep,
                          header       = input$csv_header,
                          fileEncoding = input$csv_enc,
                          stringsAsFactors = FALSE),
          xlsx = , xls = readxl::read_excel(arch$datapath),
          rds  = readRDS(arch$datapath),
          sav  = haven::read_sav(arch$datapath),
          stop("Formato no soportado: ", ext)
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error al cargar:", e$message), type = "error")
        NULL
      })

      if (!is.null(df) && is.data.frame(df)) {
        df <- as.data.frame(df)
        rv$datasets[[nombre]] <- df
        rv$seleccionado       <- nombre
        active_dataset(df)
        active_name(nombre)
        history_log(history, "dataset_loaded",
                    list(name = nombre, nrow = nrow(df), ncol = ncol(df)))
        output$msg_carga <- shiny::renderText(
          paste0("\u2714 '", nombre, "' cargado (", nrow(df), " filas \u00d7 ", ncol(df), " cols)")
        )
      }
    })

    # ── Data.frames del entorno global ────────────────────────────────────────
    dfs_entorno <- shiny::reactive({
      input$btn_refresh
      objs <- ls(envir = .GlobalEnv)
      objs[sapply(objs, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
    })

    output$lista_entorno <- shiny::renderUI({
      dfs <- dfs_entorno()
      if (length(dfs) == 0)
        return(shiny::div(class = "ds-hint", "No hay data.frames en el entorno."))
      shiny::tagList(lapply(dfs, function(nm) {
        df_tmp <- get(nm, envir = .GlobalEnv)
        shiny::div(style = "margin-bottom:4px;",
          shiny::actionLink(ns(paste0("env_", nm)), nm,
                            style = "font-size:13px; text-decoration:none;"),
          shiny::span(class = "ds-badge",
                      paste0(nrow(df_tmp), "\u00d7", ncol(df_tmp)))
        )
      }))
    })

    shiny::observe({
      lapply(dfs_entorno(), function(nm) {
        shiny::observeEvent(input[[paste0("env_", nm)]], {
          df <- get(nm, envir = .GlobalEnv)
          rv$datasets[[nm]]  <- df
          rv$seleccionado    <- nm
          active_dataset(df)
          active_name(nm)
          history_log(history, "dataset_from_env", list(name = nm))
        }, ignoreInit = TRUE)
      })
    })

    # ── Lista de datasets cargados ────────────────────────────────────────────
    output$lista_datasets <- shiny::renderUI({
      if (length(rv$datasets) == 0)
        return(shiny::div(class = "ds-hint",
                          shiny::icon("info-circle"), " Ning\u00fan dataset cargado a\u00fan."))
      shiny::tagList(lapply(names(rv$datasets), function(nm) {
        df_tmp    <- rv$datasets[[nm]]
        es_activo <- isTRUE(rv$seleccionado == nm)
        shiny::div(
          class = if (es_activo) "ds-row-active" else "",
          style = "display:flex; align-items:center; gap:8px;
                   padding:6px 8px; border-radius:6px; margin-bottom:4px;
                   border:1px solid #dee2e6; cursor:pointer;",
          onclick = paste0("Shiny.setInputValue('", ns("click_ds"), "','", nm,
                           "',{priority:'event'})"),
          shiny::icon(if (es_activo) "check-circle" else "circle",
                      class = "text-primary"),
          shiny::span(nm, style = "flex:1; font-size:13px;"),
          shiny::span(class = "ds-badge",
                      paste0(nrow(df_tmp), "\u00d7", ncol(df_tmp))),
          shiny::actionButton(
            ns(paste0("rm_", nm)), label = NULL,
            icon  = shiny::icon("trash"),
            class = "btn-sm btn-outline-danger",
            style = "padding:1px 5px; font-size:11px;",
            onclick = "event.stopPropagation();"
          )
        )
      }))
    })

    shiny::observeEvent(input$click_ds, {
      nm <- input$click_ds
      if (nm %in% names(rv$datasets)) {
        rv$seleccionado <- nm
        active_dataset(rv$datasets[[nm]])
        active_name(nm)
      }
    })

    shiny::observe({
      lapply(names(rv$datasets), function(nm) {
        shiny::observeEvent(input[[paste0("rm_", nm)]], {
          rv$datasets[[nm]] <- NULL
          if (isTRUE(rv$seleccionado == nm)) {
            restantes       <- names(rv$datasets)
            rv$seleccionado <- if (length(restantes) > 0) restantes[1] else NULL
            if (!is.null(rv$seleccionado)) {
              active_dataset(rv$datasets[[rv$seleccionado]])
              active_name(rv$seleccionado)
            } else {
              active_dataset(NULL)
              active_name(NULL)
            }
          }
        }, ignoreInit = TRUE)
      })
    })

    # ── Info del dataset seleccionado ─────────────────────────────────────────
    output$info_dataset <- shiny::renderUI({
      nm <- rv$seleccionado
      shiny::req(nm)
      df <- rv$datasets[[nm]]
      shiny::req(is.data.frame(df))

      tipos <- sapply(df, function(col) {
        switch(class(col)[1],
          numeric = "num", integer = "int", character = "chr",
          factor  = "fct", logical = "lgl", Date = "date",
          POSIXct = "dttm", "otro")
      })

      shiny::tagList(
        shiny::h6(shiny::icon("info-circle"), paste0(" ", nm),
                  style = "margin-bottom:6px; font-weight:700;"),
        shiny::fluidRow(
          shiny::column(4, shiny::div(class = "ds-badge", paste(nrow(df), "filas"))),
          shiny::column(4, shiny::div(class = "ds-badge", paste(ncol(df), "columnas"))),
          shiny::column(4, shiny::div(class = "ds-badge",
            paste(sum(sapply(df, anyNA)), "cols con NA")))
        ),
        shiny::tags$hr(style = "margin:8px 0;"),
        shiny::tags$small(
          shiny::tags$b("Variables: "),
          paste(paste0(names(tipos), " (", tipos, ")"), collapse = " \u00b7 ")
        )
      )
    })

  })
}
