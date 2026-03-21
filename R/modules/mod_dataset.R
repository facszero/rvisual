#' Módulo: Panel Dataset
#'
#' Permite al usuario:
#' - Ver los data.frames disponibles en el entorno global
#' - Cargar archivos externos (CSV, Excel, RDS)
#' - Seleccionar el dataset activo de trabajo
#' - Ver un resumen rápido del dataset seleccionado

# ── UI ────────────────────────────────────────────────────────────────────
mod_dataset_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(4, 8),

    # Columna izquierda: lista de datasets + carga
    bslib::card(
      bslib::card_header(shiny::icon("database"), " Datasets en memoria"),
      shiny::actionButton(ns("btn_refresh"), "Actualizar lista",
                          icon = shiny::icon("rotate"),
                          class = "btn-sm btn-outline-primary mb-2 w-100"),
      shiny::uiOutput(ns("dataset_list")),
      bslib::card_footer(
        shiny::actionButton(ns("btn_load_file"), "Cargar archivo...",
                            icon = shiny::icon("folder-open"),
                            class = "btn-primary w-100")
      )
    ),

    # Columna derecha: resumen del dataset activo
    bslib::card(
      bslib::card_header(shiny::icon("circle-info"), " Resumen del dataset activo"),
      shiny::uiOutput(ns("dataset_summary"))
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────
mod_dataset_server <- function(id, active_dataset, active_name, history) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Lista de datasets en entorno global
    available_datasets <- shiny::reactive({
      input$btn_refresh  # trigger on refresh
      discover_datasets()
    })

    # Renderizar lista de datasets como botones seleccionables
    output$dataset_list <- shiny::renderUI({
      datasets <- available_datasets()
      if (length(datasets) == 0) {
        return(shiny::p("No hay data.frames en el entorno.",
                        class = "text-muted small"))
      }
      shiny::tagList(
        lapply(datasets, function(ds) {
          is_active <- identical(active_name(), ds$name)
          shiny::actionButton(
            inputId = ns(paste0("select_", ds$name)),
            label   = shiny::tagList(
              shiny::tags$strong(ds$name),
              shiny::tags$small(
                glue::glue(" [{ds$nrow} x {ds$ncol}]"),
                class = "text-muted"
              )
            ),
            class = paste("btn w-100 text-start mb-1",
                          if (is_active) "btn-primary" else "btn-outline-secondary")
          )
        })
      )
    })

    # Observar clicks en cada dataset de la lista
    shiny::observe({
      datasets <- available_datasets()
      lapply(datasets, function(ds) {
        shiny::observeEvent(input[[paste0("select_", ds$name)]], {
          df <- get(ds$name, envir = .GlobalEnv)
          active_dataset(df)
          active_name(ds$name)
          history_log(history, "dataset_selected", list(name = ds$name))
        }, ignoreInit = TRUE)
      })
    })

    # Resumen del dataset activo
    output$dataset_summary <- shiny::renderUI({
      df <- active_dataset()
      if (is.null(df)) {
        return(shiny::p("Seleccioná un dataset para ver su resumen.",
                        class = "text-muted"))
      }
      meta <- get_metadata(df, active_name())
      render_dataset_summary_ui(meta)
    })

    # Modal para carga de archivos
    shiny::observeEvent(input$btn_load_file, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Cargar archivo",
          shiny::fileInput(ns("file_upload"), "Seleccionar archivo",
                           accept = c(".csv", ".xlsx", ".xls", ".rds", ".sav")),
          shiny::textInput(ns("object_name"), "Nombre del objeto en R",
                           placeholder = "ej: mis_datos"),
          shiny::uiOutput(ns("file_options")),
          footer = shiny::tagList(
            shiny::modalButton("Cancelar"),
            shiny::actionButton(ns("btn_confirm_load"), "Cargar",
                                class = "btn-primary")
          )
        )
      )
    })

    # Confirmar carga de archivo
    shiny::observeEvent(input$btn_confirm_load, {
      shiny::req(input$file_upload)
      tryCatch({
        df <- load_file(
          path      = input$file_upload$datapath,
          extension = tools::file_ext(input$file_upload$name)
        )
        obj_name <- if (nchar(trimws(input$object_name)) > 0) {
          make.names(input$object_name)
        } else {
          tools::file_path_sans_ext(input$file_upload$name)
        }
        assign(obj_name, df, envir = .GlobalEnv)
        active_dataset(df)
        active_name(obj_name)
        history_log(history, "file_loaded",
                    list(file = input$file_upload$name, object = obj_name))
        shiny::removeModal()
        shiny::showNotification(
          glue::glue("'{obj_name}' cargado: {nrow(df)} filas × {ncol(df)} columnas"),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error al cargar:", e$message), type = "error")
      })
    })

  })
}

# ── Helpers de UI ─────────────────────────────────────────────────────────

render_dataset_summary_ui <- function(meta) {
  shiny::tagList(
    bslib::value_box("Filas",    meta$nrow,  theme = "primary",   full_screen = FALSE),
    bslib::value_box("Columnas", meta$ncol,  theme = "secondary", full_screen = FALSE),
    shiny::tags$hr(),
    shiny::tags$h6("Variables"),
    shiny::tags$table(
      class = "table table-sm table-striped",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Nombre"),
          shiny::tags$th("Tipo"),
          shiny::tags$th("NA%")
        )
      ),
      shiny::tags$tbody(
        lapply(seq_len(nrow(meta$columns)), function(i) {
          col <- meta$columns[i, ]
          shiny::tags$tr(
            shiny::tags$td(col$name),
            shiny::tags$td(shiny::tags$code(col$type)),
            shiny::tags$td(sprintf("%.1f%%", col$na_pct))
          )
        })
      )
    )
  )
}
