#' M\u00f3dulo: Constructor Visual de Operaciones

# -- UI --------------------------------------------------------------------
mod_builder_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(3, 9),

    bslib::card(
      bslib::card_header("Operaciones"),
      shiny::tags$div(class = "d-grid gap-2",
        shiny::actionButton(ns("op_select"),  "Seleccionar columnas", icon = shiny::icon("columns"),       class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_filter"),  "Filtrar registros",    icon = shiny::icon("filter"),        class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_arrange"), "Ordenar",              icon = shiny::icon("sort"),          class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_group"),   "Agrupar y resumir",    icon = shiny::icon("layer-group"),   class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_mutate"),  "Crear variable",       icon = shiny::icon("plus-circle"),   class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_rename"),  "Renombrar columna",    icon = shiny::icon("pen"),           class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_recode"),  "Recodificar valores",  icon = shiny::icon("arrows-rotate"), class = "btn-outline-primary text-start"),
        shiny::actionButton(ns("op_join"),    "Unir tablas (join)",   icon = shiny::icon("link"),          class = "btn-outline-primary text-start")
      ),
      bslib::card_footer(
        shiny::actionButton(ns("btn_clear_ops"), "Limpiar todo",
                            icon = shiny::icon("trash"), class = "btn-outline-danger btn-sm w-100")
      )
    ),

    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Operaciones aplicadas",
          bslib::tooltip(shiny::icon("circle-info"),
            "Cada operaci\u00f3n que agreg\u00e1s se apila aqu\u00ed. Pod\u00e9s eliminar cualquiera.")
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

# -- Server ----------------------------------------------------------------
mod_builder_server <- function(id, active_dataset, active_name,
                                operation_stack, generated_code, history) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Helper: columnas del dataset activo -------------------------------
    cols <- shiny::reactive({
      df <- active_dataset()
      if (is.null(df)) character(0) else names(df)
    })
    cols_num <- shiny::reactive({
      df <- active_dataset()
      if (is.null(df)) character(0)
      else names(df)[sapply(df, is.numeric)]
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 1. SELECCIONAR COLUMNAS
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    shiny::observeEvent(input$op_select, {
      shiny::req(active_dataset())
      shiny::showModal(shiny::modalDialog(
        title = "Seleccionar columnas",
        shiny::checkboxGroupInput(ns("sel_cols"), "Columnas a conservar:",
          choices = cols(), selected = cols()),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_select"), "Aplicar", class = "btn-primary")
        )
      ))
    })
    shiny::observeEvent(input$confirm_select, {
      shiny::req(input$sel_cols)
      push_operation(operation_stack, history, op_select(input$sel_cols))
      shiny::removeModal()
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 2. FILTRAR REGISTROS
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    shiny::observeEvent(input$op_filter, {
      shiny::req(active_dataset())
      shiny::showModal(shiny::modalDialog(
        title = "Filtrar registros",
        shiny::selectInput(ns("filter_col"), "Variable:", choices = cols()),
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
          shiny::selectInput(ns("filter_op"), "Condici\u00f3n:",
            choices = c("==" , "!=", ">", "<", ">=", "<=")),
          shiny::numericInput(ns("filter_val"), "Valor:", value = 0)
        )
      } else {
        shiny::tagList(
          shiny::selectInput(ns("filter_op"), "Condici\u00f3n:",
            choices = c("igual a" = "==", "distinto de" = "!=")),
          shiny::selectInput(ns("filter_val"), "Valor:",
            choices = unique(na.omit(col)))
        )
      }
    })
    shiny::observeEvent(input$confirm_filter, {
      shiny::req(input$filter_col, input$filter_op, input$filter_val)
      push_operation(operation_stack, history,
                     op_filter(input$filter_col, input$filter_op, input$filter_val))
      shiny::removeModal()
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 3. ORDENAR
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    shiny::observeEvent(input$op_arrange, {
      shiny::req(active_dataset())
      shiny::showModal(shiny::modalDialog(
        title = "Ordenar",
        shiny::selectInput(ns("arrange_col"), "Ordenar por:", choices = cols()),
        shiny::radioButtons(ns("arrange_dir"), "Direcci\u00f3n:",
          choices = c("Ascendente" = "asc", "Descendente" = "desc"),
          inline  = TRUE),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_arrange"), "Aplicar", class = "btn-primary")
        )
      ))
    })
    shiny::observeEvent(input$confirm_arrange, {
      shiny::req(input$arrange_col)
      push_operation(operation_stack, history,
                     op_arrange(input$arrange_col, desc = input$arrange_dir == "desc"))
      shiny::removeModal()
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 4. AGRUPAR Y RESUMIR
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    shiny::observeEvent(input$op_group, {
      shiny::req(active_dataset())
      shiny::showModal(shiny::modalDialog(
        title = "Agrupar y resumir",
        size  = "l",
        shiny::fluidRow(
          shiny::column(6,
            shiny::tags$strong("1. Columnas de agrupaci\u00f3n:"),
            shiny::checkboxGroupInput(ns("group_cols"), NULL,
              choices = cols())
          ),
          shiny::column(6,
            shiny::tags$strong("2. Calcular:"),
            shiny::tags$p(shiny::tags$small("Pod\u00e9s agregar m\u00faltiples c\u00e1lculos.", class = "text-muted")),
            shiny::uiOutput(ns("group_summary_rows")),
            shiny::actionButton(ns("add_summary_row"), shiny::icon("plus"),
                                label = " Agregar c\u00e1lculo",
                                class = "btn-sm btn-outline-secondary mt-2")
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_group"), "Aplicar", class = "btn-primary")
        )
      ))
      # Inicializar con 1 fila de resumen
      rv_group$n_rows <- 1
    })

    rv_group <- shiny::reactiveValues(n_rows = 1)

    shiny::observeEvent(input$add_summary_row, {
      rv_group$n_rows <- rv_group$n_rows + 1
    })

    output$group_summary_rows <- shiny::renderUI({
      shiny::tagList(lapply(seq_len(rv_group$n_rows), function(i) {
        shiny::div(class = "d-flex gap-2 mb-2 align-items-end",
          shiny::textInput(ns(paste0("sum_newcol_", i)), if (i == 1) "Nombre resultado" else NULL,
                           placeholder = paste0("col_res_", i), width = "130px"),
          shiny::selectInput(ns(paste0("sum_fn_", i)), if (i == 1) "Funci\u00f3n" else NULL,
            choices = c("Media" = "mean", "Suma" = "sum", "M\u00ednimo" = "min",
                        "M\u00e1ximo" = "max", "Conteo" = "n", "Desv\u00edo" = "sd",
                        "Mediana" = "median"),
            width = "110px"),
          shiny::selectInput(ns(paste0("sum_col_", i)), if (i == 1) "Variable" else NULL,
            choices = c("(conteo)" = "__n__", cols()), width = "150px")
        )
      }))
    })

    shiny::observeEvent(input$confirm_group, {
      shiny::req(input$group_cols)
      n <- rv_group$n_rows
      summary_fns <- list()
      for (i in seq_len(n)) {
        new_col <- trimws(input[[paste0("sum_newcol_", i)]])
        fn      <- input[[paste0("sum_fn_",     i)]]
        src_col <- input[[paste0("sum_col_",    i)]]
        if (nchar(new_col) == 0) new_col <- paste0("res_", i)
        if (!is.null(fn)) {
          summary_fns[[make.names(new_col)]] <- list(
            fn     = fn,
            col    = if (fn == "n" || src_col == "__n__") NULL else src_col,
            na_rm  = TRUE
          )
        }
      }
      if (length(summary_fns) > 0) {
        push_operation(operation_stack, history,
                       op_group_summarise(input$group_cols, summary_fns))
      }
      shiny::removeModal()
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 5. CREAR VARIABLE (mutate)
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    shiny::observeEvent(input$op_mutate, {
      shiny::req(active_dataset())
      shiny::showModal(shiny::modalDialog(
        title = "Crear nueva variable",
        shiny::textInput(ns("mutate_newcol"), "Nombre de la nueva variable:",
                         placeholder = "ej: precio_total"),
        shiny::textAreaInput(ns("mutate_expr"), "Expresi\u00f3n R:",
          placeholder = "ej: Precio * Cantidad\n    o: ifelse(Marca == 'X', 1, 0)",
          rows = 3),
        shiny::tags$small(class = "text-muted",
          "Variables disponibles: ",
          shiny::tags$code(paste(cols(), collapse = ", "))
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_mutate"), "Aplicar", class = "btn-primary")
        )
      ))
    })
    shiny::observeEvent(input$confirm_mutate, {
      shiny::req(input$mutate_newcol, input$mutate_expr)
      push_operation(operation_stack, history,
                     op_mutate(trimws(input$mutate_newcol), trimws(input$mutate_expr)))
      shiny::removeModal()
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 6. RENOMBRAR COLUMNA
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    shiny::observeEvent(input$op_rename, {
      shiny::req(active_dataset())
      shiny::showModal(shiny::modalDialog(
        title = "Renombrar columna",
        shiny::selectInput(ns("rename_old"), "Columna a renombrar:", choices = cols()),
        shiny::textInput(ns("rename_new"), "Nuevo nombre:",
                         placeholder = "ej: precio_unitario"),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_rename"), "Aplicar", class = "btn-primary")
        )
      ))
    })
    shiny::observeEvent(input$confirm_rename, {
      shiny::req(input$rename_old, input$rename_new)
      push_operation(operation_stack, history,
                     op_rename(input$rename_old, trimws(input$rename_new)))
      shiny::removeModal()
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 7. RECODIFICAR VALORES
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    shiny::observeEvent(input$op_recode, {
      shiny::req(active_dataset())
      shiny::showModal(shiny::modalDialog(
        title = "Recodificar valores",
        shiny::selectInput(ns("recode_col"), "Variable a recodificar:", choices = cols()),
        shiny::uiOutput(ns("recode_values_ui")),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_recode"), "Aplicar", class = "btn-primary")
        )
      ))
    })
    output$recode_values_ui <- shiny::renderUI({
      shiny::req(input$recode_col, active_dataset())
      vals <- unique(na.omit(active_dataset()[[input$recode_col]]))
      vals <- head(as.character(vals), 20)  # m\u00e1x 20 valores
      shiny::tagList(
        shiny::tags$p(shiny::tags$small(
          glue::glue("{length(vals)} valores \u00fanicos (m\u00e1x. 20 mostrados):"),
          class = "text-muted")),
        lapply(vals, function(v) {
          shiny::div(class = "d-flex gap-2 mb-1 align-items-center",
            shiny::tags$span(v, style = "width:150px; font-size:13px;"),
            shiny::tags$span("\u2192", style = "color:#888"),
            shiny::textInput(ns(paste0("recode_new_", make.names(v))),
                             label = NULL, value = v, width = "150px")
          )
        })
      )
    })
    shiny::observeEvent(input$confirm_recode, {
      shiny::req(input$recode_col, active_dataset())
      vals <- head(as.character(unique(na.omit(active_dataset()[[input$recode_col]]))), 20)
      mapping <- stats::setNames(
        lapply(vals, function(v) {
          nuevo <- input[[paste0("recode_new_", make.names(v))]]
          if (is.null(nuevo)) v else nuevo
        }),
        vals
      )
      # Solo guardar los que cambiaron
      mapping <- mapping[vapply(names(mapping), function(k) mapping[[k]] != k, logical(1))]
      if (length(mapping) > 0) {
        push_operation(operation_stack, history,
                       op_recode(input$recode_col, mapping))
      }
      shiny::removeModal()
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 8. JOIN
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    shiny::observeEvent(input$op_join, {
      shiny::req(active_dataset())
      dfs_disponibles <- ls(envir = .GlobalEnv)[
        sapply(ls(envir = .GlobalEnv),
               function(x) is.data.frame(get(x, envir = .GlobalEnv)))
      ]
      shiny::showModal(shiny::modalDialog(
        title = "Unir tablas (join)",
        shiny::selectInput(ns("join_right_df"), "Segundo dataset:",
          choices = dfs_disponibles),
        shiny::selectInput(ns("join_type"), "Tipo de join:",
          choices = c("Left join" = "left", "Inner join" = "inner",
                      "Right join" = "right", "Full join" = "full")),
        shiny::textInput(ns("join_by"), "Columnas clave (separadas por coma):",
          placeholder = "ej: id_producto, codigo"),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_join"), "Aplicar", class = "btn-primary")
        )
      ))
    })
    shiny::observeEvent(input$confirm_join, {
      shiny::req(input$join_right_df, input$join_by)
      by_cols <- trimws(strsplit(input$join_by, ",")[[1]])
      push_operation(operation_stack, history,
                     op_join(input$join_right_df, by_cols, input$join_type))
      shiny::removeModal()
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # LIMPIAR STACK
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    shiny::observeEvent(input$btn_clear_ops, {
      operation_stack(list())
      generated_code("")
      history_log(history, "stack_cleared", list())
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # RENDERIZAR STACK
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    output$operation_stack_ui <- shiny::renderUI({
      ops <- operation_stack()
      if (length(ops) == 0)
        return(shiny::p("A\u00fan no hay operaciones. Eleg\u00ed una del panel izquierdo.",
                        class = "text-muted"))
      shiny::tagList(lapply(seq_along(ops), function(i) {
        shiny::div(
          class = "d-flex align-items-center justify-content-between mb-1 p-2 border rounded",
          shiny::span(shiny::tags$small(glue::glue("{i}. {ops[[i]]$label}"))),
          shiny::actionButton(ns(paste0("remove_op_", i)), label = NULL,
                              icon = shiny::icon("x"),
                              class = "btn-sm btn-outline-danger")
        )
      }))
    })

    shiny::observe({
      ops <- operation_stack()
      lapply(seq_along(ops), function(i) {
        shiny::observeEvent(input[[paste0("remove_op_", i)]], {
          current <- operation_stack()
          if (i <= length(current)) operation_stack(current[-i])
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # VISTA PREVIA
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
        head(preview_data(), 200),
        options  = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class    = "table table-sm"
      )
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # GENERAR C\u00d3DIGO
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    shiny::observe({
      df_name <- active_name()
      ops     <- operation_stack()
      shiny::req(df_name)
      generated_code(generate_code(df_name, ops))
    })

  })
}

# -- Helper ----------------------------------------------------------------
push_operation <- function(operation_stack, history, op) {
  operation_stack(c(shiny::isolate(operation_stack()), list(op)))
  history_log(history, "operation_added", list(type = op$type, label = op$label))
}
