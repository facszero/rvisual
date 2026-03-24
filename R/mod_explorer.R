#' M\u00f3dulo: Explorador de datos
#'
#' Vista tabular interactiva del dataset activo.
#' Incluye:
#' - Tabla paginada con DT
#' - Resumen de columnas con tipos detectados
#' - Estad\u00edsticas r\u00e1pidas por variable
#' - B\u00fasqueda de variables

# -- UI --------------------------------------------------------------------
mod_explorer_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::navset_card_tab(
    # Tab 1: Vista de datos
    bslib::nav_panel(
      "Vista de datos",
      shiny::uiOutput(ns("no_data_msg")),
      DT::DTOutput(ns("data_table"))
    ),
    # Tab 2: Columnas y tipos
    bslib::nav_panel(
      "Columnas",
      shiny::textInput(ns("col_search"), "Buscar variable", placeholder = "Nombre..."),
      DT::DTOutput(ns("columns_table"))
    ),
    # Tab 3: Estad\u00edsticas r\u00e1pidas
    bslib::nav_panel(
      "Estad\u00edsticas",
      shiny::selectInput(ns("sel_col_stats"), "Seleccionar variable", choices = NULL),
      shiny::uiOutput(ns("col_stats"))
    )
  )
}

# -- Server ----------------------------------------------------------------
mod_explorer_server <- function(id, active_dataset, active_name) {
  shiny::moduleServer(id, function(input, output, session) {

    output$no_data_msg <- shiny::renderUI({
      if (is.null(active_dataset())) {
        shiny::div(class = "alert alert-info mt-3",
                   "Carg\u00e1 o seleccion\u00e1 un dataset en el panel 'Dataset'.")
      }
    })

    # Tabla principal paginada
    output$data_table <- DT::renderDT({
      shiny::req(active_dataset())
      DT::datatable(
        active_dataset(),
        options = list(
          pageLength = 20,
          scrollX    = TRUE,
          dom        = "frtip"
        ),
        rownames = FALSE,
        class    = "table table-sm table-striped"
      )
    })

    # Tabla de columnas con info de tipos
    output$columns_table <- DT::renderDT({
      shiny::req(active_dataset())
      df   <- active_dataset()
      meta <- get_metadata(df, active_name())
      cols_df <- meta$columns

      # Filtro de b\u00fasqueda
      if (nchar(trimws(input$col_search)) > 0) {
        cols_df <- cols_df[grepl(input$col_search, cols_df$name, ignore.case = TRUE), ]
      }

      DT::datatable(
        cols_df,
        options  = list(pageLength = 30, dom = "tp"),
        rownames = FALSE,
        colnames = c("Variable", "Tipo R", "Tipo Visual", "\u00danicos", "NA%", "Ejemplo")
      )
    })

    # Actualizar selector de variables para estad\u00edsticas
    shiny::observe({
      shiny::req(active_dataset())
      shiny::updateSelectInput(session, "sel_col_stats",
                               choices = names(active_dataset()))
    })

    # Estad\u00edsticas de columna seleccionada
    output$col_stats <- shiny::renderUI({
      shiny::req(active_dataset(), input$sel_col_stats)
      col_name <- input$sel_col_stats
      df       <- active_dataset()
      shiny::req(col_name %in% names(df))
      render_col_stats_ui(df[[col_name]], col_name)
    })

  })
}

# -- Helpers ---------------------------------------------------------------

render_col_stats_ui <- function(col_vector, col_name) {
  # TODO: expandir con visualizaciones (histograma, barras de frecuencia, etc.)
  is_numeric_col <- is.numeric(col_vector)

  if (is_numeric_col) {
    stats <- summary(col_vector)
    shiny::tagList(
      shiny::tags$h6(col_name, class = "mt-3"),
      shiny::tags$ul(class = "list-group list-group-flush",
        shiny::tags$li(class = "list-group-item", glue::glue("M\u00ednimo: {stats['Min.']}")),
        shiny::tags$li(class = "list-group-item", glue::glue("Mediana: {stats['Median']}")),
        shiny::tags$li(class = "list-group-item", glue::glue("Media: {round(stats['Mean'], 4)}")),
        shiny::tags$li(class = "list-group-item", glue::glue("M\u00e1ximo: {stats['Max.']}")),
        shiny::tags$li(class = "list-group-item",
                       glue::glue("Valores NA: {sum(is.na(col_vector))}"))
      )
    )
  } else {
    freqs <- sort(table(col_vector, useNA = "ifany"), decreasing = TRUE)
    top_n <- head(freqs, 10)
    shiny::tagList(
      shiny::tags$h6(col_name, class = "mt-3"),
      shiny::tags$p(glue::glue("Categor\u00edas \u00fanicas: {length(freqs)}")),
      DT::renderDT(
        as.data.frame(top_n),
        options  = list(pageLength = 10, dom = "t"),
        rownames = FALSE,
        colnames = c("Valor", "Frecuencia")
      )
    )
  }
}
