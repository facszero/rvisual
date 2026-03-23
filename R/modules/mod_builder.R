#' Módulo: Constructor Visual de Operaciones
#'
#' Interfaz punto-a-click para construir transformaciones sobre el dataset activo.
#' Cada acción del usuario genera una entrada en el `operation_stack`,
#' que luego el `code_generator` convierte en código R limpio.
#'
#' Operaciones disponibles en MVP:
#'   select, filter, arrange, group_by + summarise, mutate, rename, recode, join

# ── UI ────────────────────────────────────────────────────────────────────
mod_builder_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(3, 9),

    # Panel izquierdo: menú de operaciones
    bslib::card(
      bslib::card_header("Operaciones"),
      shiny::tags$div(class = "d-grid gap-2",
        shiny::actionButton(ns("op_select"),    "Seleccionar columnas", icon = shiny::icon("columns"),       class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_filter"),    "Filtrar registros",    icon = shiny::icon("filter"),        class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_arrange"),   "Ordenar",              icon = shiny::icon("sort"),          class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_group"),     "Agrupar y resumir",    icon = shiny::icon("layer-group"),   class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_mutate"),    "Crear variable",       icon = shiny::icon("plus-circle"),   class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_rename"),    "Renombrar columna",    icon = shiny::icon("pen"),           class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_recode"),    "Recodificar valores",  icon = shiny::icon("arrows-rotate"), class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_join"),      "Unir tablas (join)",   icon = shiny::icon("link"),          class = "btn-outline-primary text-start")
      ),
      bslib::card_footer(
        shiny::actionButton(ns("btn_clear_ops"), "Limpiar todo",
                            icon  = shiny::icon("trash"),
                            class = "btn-outline-danger btn-sm w-100")
      )
    ),

    # Panel derecho: stack de operaciones + preview
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header(
          "Operaciones aplicadas",
          bslib::tooltip(
            shiny::icon("circle-info"),
            "Cada operación que agregás se apila aquí. Podés eliminar cualquiera."
          )
        ),
        shiny::uiOutput(ns("operation_stack_ui"))
      ),
      bslib::card(
        bslib::card_header("Vista previa del resultado"),
        DT::DTOutput(ns("preview_table"))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────
mod_builder_server <- function(id, active_dataset, active_name,
                                operation_stack, generated_code, history) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Operación: Seleccionar columnas ───────────────────────────────
    shiny::observeEvent(input$op_select, {
      shiny::req(active_dataset())
      shiny::showModal(shiny::modalDialog(
        title = "Seleccionar columnas",
        shiny::checkboxGroupInput(
          ns("sel_cols"),
          "Columnas a conservar:",
          choices  = names(active_dataset()),
          selected = names(active_dataset())
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_select"), "Aplicar", class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$confirm_select, {
      shiny::req(input$sel_cols)
      push_operation(operation_stack, history, op = op_select(input$sel_cols))
      shiny::removeModal()
    })

    # ── Operación: Filtrar ────────────────────────────────────────────
    shiny::observeEvent(input$op_filter, {
      shiny::req(active_dataset())
      shiny::showModal(shiny::modalDialog(
        title = "Filtrar registros",
        shiny::selectInput(ns("filter_col"), "Variable a filtrar:",
                           choices = names(active_dataset())),
        shiny::uiOutput(ns("filter_value_ui")),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_filter"), "Aplicar", class = "btn-primary")
        )
      ))
    })

    output$filter_value_ui <- shiny::renderUI({
      shiny::req(input$filter_col, active_dataset())
      col <- active_dataset()[[input$filter_col]]
      if (is.numeric(col)) {
        shiny::tagList(
          shiny::selectInput(ns("filter_op"), "Condición:",
                             choices = c("igual a" = "==", "mayor que" = ">",
                                         "menor que" = "<", "mayor o igual" = ">=",
                                         "menor o igual" = "<=")),
          shiny::numericInput(ns("filter_val"), "Valor:", value = 0)
        )
      } else {
        shiny::tagList(
          shiny::selectInput(ns("filter_op"), "Condición:",
                             choices = c("igual a" = "==", "distinto de" = "!=")),
          shiny::selectInput(ns("filter_val"), "Valor:",
                             choices = unique(na.omit(col)))
        )
      }
    })

    shiny::observeEvent(input$confirm_filter, {
      shiny::req(input$filter_col, input$filter_op, input$filter_val)
      push_operation(operation_stack, history,
                     op = op_filter(input$filter_col, input$filter_op, input$filter_val))
      shiny::removeModal()
    })

    # ── Limpiar stack ─────────────────────────────────────────────────
    shiny::observeEvent(input$btn_clear_ops, {
      operation_stack(list())
      generated_code("")
      history_log(history, "stack_cleared", list())
    })

    # ── Renderizar stack de operaciones ───────────────────────────────
    output$operation_stack_ui <- shiny::renderUI({
      ops <- operation_stack()
      if (length(ops) == 0) {
        return(shiny::p("Aún no hay operaciones. Elegí una del panel izquierdo.",
                        class = "text-muted"))
      }
      shiny::tagList(
        lapply(seq_along(ops), function(i) {
          op <- ops[[i]]
          shiny::div(
            class = "d-flex align-items-center justify-content-between mb-1 p-2 border rounded",
            shiny::span(shiny::tags$small(glue::glue("{i}. {op$label}"))),
            shiny::actionButton(
              ns(paste0("remove_op_", i)),
              label = NULL, icon = shiny::icon("x"),
              class = "btn-sm btn-outline-danger"
            )
          )
        })
      )
    })

    # Observar botones de eliminar operación
    shiny::observe({
      ops <- operation_stack()
      lapply(seq_along(ops), function(i) {
        shiny::observeEvent(input[[paste0("remove_op_", i)]], {
          current <- operation_stack()
          operation_stack(current[-i])
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # ── Vista previa reactiva ─────────────────────────────────────────
    preview_data <- shiny::reactive({
      df  <- active_dataset()
      ops <- operation_stack()
      shiny::req(df)
      tryCatch(
        apply_operations(df, ops),
        error = function(e) {
          shiny::showNotification(paste("Error en preview:", e$message), type = "warning")
          df
        }
      )
    })

    output$preview_table <- DT::renderDT({
      shiny::req(preview_data())
      DT::datatable(
        head(preview_data(), 100),
        options  = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class    = "table table-sm"
      )
    })

    # ── Generar código cuando cambia el stack ─────────────────────────
    shiny::observe({
      df_name <- active_name()
      ops     <- operation_stack()
      shiny::req(df_name)
      code <- generate_code(df_name, ops)
      generated_code(code)
    })

  })
}

# ── Helpers internos ──────────────────────────────────────────────────────

push_operation <- function(operation_stack, history, op) {
  current <- operation_stack()
  operation_stack(c(current, list(op)))
  history_log(history, "operation_added", op)
}
