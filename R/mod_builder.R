#' Modulo: Constructor Visual de Operaciones
#'
#' @param id ID del modulo Shiny.
#' @return Un objeto shiny.tag con la UI del constructor.

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
                                operation_stack, generated_code, history,
                                active_source = NULL) {
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
    # Seleccion actual y filtro actual
    sel_cols_selected <- shiny::reactiveVal(character(0))
    sel_cols_filter   <- shiny::reactiveVal(character(0))

    shiny::observeEvent(input$op_select, {
      shiny::req(active_dataset())
      todas <- cols()
      sel_cols_selected(todas)
      sel_cols_filter(todas)
      shiny::showModal(shiny::modalDialog(
        title = "Seleccionar columnas",
        size  = "l",
        shiny::div(class = "d-flex gap-2 mb-3 align-items-center",
          shiny::div(style = "flex:1;",
            shiny::textInput(ns("sel_search"), NULL,
              placeholder = "Buscar columna...", width = "100%")
          ),
          shiny::actionButton(ns("sel_all"),  "Marcar todas",
            class = "btn-sm btn-outline-secondary"),
          shiny::actionButton(ns("sel_none"), "Desmarcar todas",
            class = "btn-sm btn-outline-secondary")
        ),
        shiny::div(style = "max-height:420px; overflow-y:auto;",
          shiny::checkboxGroupInput(ns("sel_cols"), NULL,
            choices  = todas,
            selected = todas)
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_select"), "Aplicar", class = "btn-primary")
        )
      ))
      shiny::updateTextInput(session, "sel_search", value = "")
    })

    # Guardar cambios manuales de checkboxes en sel_cols_selected
    shiny::observeEvent(input$sel_cols, {
      cf        <- sel_cols_filter()
      hidden    <- setdiff(sel_cols_selected(), cf)
      visible   <- if (!is.null(input$sel_cols)) input$sel_cols else character(0)
      sel_cols_selected(union(hidden, visible))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Filtrar columnas al escribir en el buscador
    shiny::observeEvent(input$sel_search, {
      todas    <- cols()
      busqueda <- trimws(if (!is.null(input$sel_search)) input$sel_search else "")
      filtradas <- if (nchar(busqueda) > 0)
        todas[grepl(busqueda, todas, ignore.case = TRUE)]
      else todas
      sel_cols_filter(filtradas)
      shiny::updateCheckboxGroupInput(session, "sel_cols",
        choices  = filtradas,
        selected = intersect(sel_cols_selected(), filtradas))
    }, ignoreInit = TRUE)

    # Marcar todas las visibles
    shiny::observeEvent(input$sel_all, {
      cf <- sel_cols_filter()
      sel_cols_selected(union(sel_cols_selected(), cf))
      shiny::updateCheckboxGroupInput(session, "sel_cols",
        choices = cf, selected = cf)
    })

    # Desmarcar todas las visibles
    shiny::observeEvent(input$sel_none, {
      cf <- sel_cols_filter()
      sel_cols_selected(setdiff(sel_cols_selected(), cf))
      shiny::updateCheckboxGroupInput(session, "sel_cols",
        choices = cf, selected = character(0))
    })

    shiny::observeEvent(input$confirm_select, {
      seleccion <- intersect(cols(), sel_cols_selected())
      shiny::req(length(seleccion) > 0)
      push_operation(operation_stack, history, op_select(seleccion))
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
                     op_filter(input$filter_col, input$filter_op, input$filter_val,
                               numeric = is.numeric(active_dataset()[[input$filter_col]])))
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
    grp_cols_selected <- shiny::reactiveVal(character(0))
    grp_cols_filter   <- shiny::reactiveVal(character(0))

    shiny::observeEvent(input$op_group, {
      shiny::req(active_dataset())
      todas <- cols()
      grp_cols_selected(character(0))  # ninguna seleccionada por defecto
      grp_cols_filter(todas)
      shiny::showModal(shiny::modalDialog(
        title = "Agrupar y resumir",
        size  = "l",
        shiny::fluidRow(
          shiny::column(6,
            shiny::tags$strong("1. Columnas de agrupaci\u00f3n:"),
            # Buscador de columnas de agrupacion
            shiny::div(class = "d-flex gap-2 my-2 align-items-center",
              shiny::div(style = "flex:1;",
                shiny::textInput(ns("grp_search"), NULL,
                  placeholder = "Buscar columna...", width = "100%")
              ),
              shiny::actionButton(ns("grp_all"),  "Todas",
                class = "btn-sm btn-outline-secondary"),
              shiny::actionButton(ns("grp_none"), "Ninguna",
                class = "btn-sm btn-outline-secondary")
            ),
            shiny::div(style = "max-height:320px; overflow-y:auto;",
              shiny::checkboxGroupInput(ns("group_cols"), NULL,
                choices = todas, selected = character(0))
            )
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
      rv_group$n_rows <- 1
      shiny::updateTextInput(session, "grp_search", value = "")
    })

    # Guardar cambios manuales en group_cols
    shiny::observeEvent(input$group_cols, {
      cf      <- grp_cols_filter()
      hidden  <- setdiff(grp_cols_selected(), cf)
      visible <- if (!is.null(input$group_cols)) input$group_cols else character(0)
      grp_cols_selected(union(hidden, visible))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Filtrar columnas de agrupacion al buscar
    shiny::observeEvent(input$grp_search, {
      todas    <- cols()
      busqueda <- trimws(if (!is.null(input$grp_search)) input$grp_search else "")
      filtradas <- if (nchar(busqueda) > 0)
        todas[grepl(busqueda, todas, ignore.case = TRUE)]
      else todas
      grp_cols_filter(filtradas)
      shiny::updateCheckboxGroupInput(session, "group_cols",
        choices  = filtradas,
        selected = intersect(grp_cols_selected(), filtradas))
    }, ignoreInit = TRUE)

    # Marcar todas las visibles en agrupacion
    shiny::observeEvent(input$grp_all, {
      cf <- grp_cols_filter()
      grp_cols_selected(union(grp_cols_selected(), cf))
      shiny::updateCheckboxGroupInput(session, "group_cols",
        choices = cf, selected = cf)
    })

    # Desmarcar todas las visibles en agrupacion
    shiny::observeEvent(input$grp_none, {
      cf <- grp_cols_filter()
      grp_cols_selected(setdiff(grp_cols_selected(), cf))
      shiny::updateCheckboxGroupInput(session, "group_cols",
        choices = cf, selected = character(0))
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
      seleccion <- intersect(cols(), grp_cols_selected())
      # seleccion puede ser vacio: summarise sin group_by es valido (agregacion global)
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
                       op_group_summarise(seleccion, summary_fns))
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
                       op_recode(input$recode_col, mapping,
                                 numeric = is.numeric(active_dataset()[[input$recode_col]])))
      }
      shiny::removeModal()
    })

    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # 8. JOIN
    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Tabla izquierda del join: el resultado de las operaciones ya apiladas
    # (el join se aplica despues de ellas)
    join_left <- shiny::reactive({
      df <- active_dataset()
      shiny::req(df)
      tryCatch(apply_operations(df, operation_stack()), error = function(e) df)
    })

    join_right <- shiny::reactive({
      shiny::req(input$join_right_df)
      tryCatch(rvisual_find_dataset(input$join_right_df), error = function(e) NULL)
    })

    shiny::observeEvent(input$op_join, {
      shiny::req(active_dataset())
      candidatos <- rvisual_join_candidates()
      if (length(candidatos) == 0) {
        shiny::showNotification(
          "No hay otro dataset para unir. Carg\u00e1 uno en la pesta\u00f1a Dataset.",
          type = "warning")
        return()
      }
      left_cols <- names(join_left())
      shiny::showModal(shiny::modalDialog(
        title = "Unir tablas (join)",
        size  = "l",
        shiny::selectInput(ns("join_right_df"), "Segundo dataset:",
          choices = candidatos),
        shiny::selectInput(ns("join_type"), "Tipo de join:",
          choices = c("Left join" = "left", "Inner join" = "inner",
                      "Right join" = "right", "Full join" = "full")),
        shiny::tags$hr(),
        shiny::tags$strong("Columnas de uni\u00f3n:"),
        shiny::tags$p(shiny::tags$small(
          "Pod\u00e9s especificar columnas con distinto nombre en cada tabla.",
          class = "text-muted")),
        shiny::fluidRow(
          shiny::column(5,
            shiny::selectInput(ns("join_col_left"), "Columna (tabla izquierda):",
              choices = left_cols)
          ),
          shiny::column(2,
            shiny::tags$div(style = "padding-top:32px; text-align:center;",
              shiny::tags$strong("="))
          ),
          shiny::column(5,
            shiny::selectInput(ns("join_col_right"), "Columna (tabla derecha):",
              choices = character(0))
          )
        ),
        shiny::uiOutput(ns("join_more_cols_ui")),
        shiny::actionButton(ns("join_add_col"), shiny::icon("plus"),
          label = " Agregar otra columna clave",
          class = "btn-sm btn-outline-secondary mb-2"),
        shiny::uiOutput(ns("join_check_ui")),
        shiny::tags$hr(),
        shiny::selectizeInput(ns("join_right_cols"),
          "Columnas a traer de la tabla derecha:",
          choices = character(0), multiple = TRUE, width = "100%",
          options = list(plugins = list("remove_button"))),
        shiny::tags$div(class = "d-flex gap-2 mb-1",
          shiny::actionLink(ns("join_cols_all"), "Todas"),
          shiny::tags$span("\u00b7", class = "text-muted"),
          shiny::actionLink(ns("join_cols_new"),
                            "Solo las que no est\u00e1n en la tabla izquierda")
        ),
        shiny::uiOutput(ns("join_dup_ui")),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_join"), "Aplicar", class = "btn-primary")
        )
      ))
      rv_join$n_cols <- 1
    })

    # Claves elegidas: c(col_izquierda = col_derecha)
    join_by <- shiny::reactive({
      n <- rv_join$n_cols
      by_left  <- c(input$join_col_left %||% "",
        if (n > 1) vapply(2:n, function(i) input[[paste0("join_col_left_",  i)]] %||% "", character(1)))
      by_right <- c(input$join_col_right %||% "",
        if (n > 1) vapply(2:n, function(i) input[[paste0("join_col_right_", i)]] %||% "", character(1)))
      valid <- nchar(by_left) > 0 & nchar(by_right) > 0
      stats::setNames(by_right[valid], by_left[valid])
    })

    # Columnas no clave de la tabla derecha
    join_right_nonkey <- shiny::reactive({
      rdf <- join_right()
      if (is.null(rdf)) return(character(0))
      setdiff(names(rdf), unname(join_by()))
    })

    # Al cambiar de dataset derecho: claves y columnas a traer
    shiny::observeEvent(input$join_right_df, {
      rdf <- join_right()
      right_cols <- if (is.null(rdf)) character(0) else names(rdf)
      # Si hay una columna con el mismo nombre que la clave izquierda, proponerla
      sel <- if (isTRUE(input$join_col_left %in% right_cols)) input$join_col_left else NULL
      shiny::updateSelectInput(session, "join_col_right", choices = right_cols,
                               selected = sel)
      shiny::updateSelectizeInput(session, "join_right_cols",
        choices = right_cols, selected = right_cols)
    })

    # Las claves derechas se traen siempre: no se ofrecen como columnas
    shiny::observeEvent(join_right_nonkey(), {
      actuales <- input$join_right_cols
      opciones <- join_right_nonkey()
      sel <- if (is.null(actuales)) opciones else intersect(actuales, opciones)
      shiny::updateSelectizeInput(session, "join_right_cols",
        choices = opciones, selected = sel)
    })

    shiny::observeEvent(input$join_cols_all, {
      shiny::updateSelectizeInput(session, "join_right_cols",
        selected = join_right_nonkey())
    })

    shiny::observeEvent(input$join_cols_new, {
      shiny::updateSelectizeInput(session, "join_right_cols",
        selected = setdiff(join_right_nonkey(), names(join_left())))
    })

    # Aviso de columnas repetidas (dplyr les agrega .x / .y)
    output$join_dup_ui <- shiny::renderUI({
      dup <- intersect(input$join_right_cols, names(join_left()))
      if (length(dup) == 0) return(NULL)
      shiny::tags$small(class = "text-muted", paste0(
        length(dup), " columna(s) existen en las dos tablas y quedar\u00e1n ",
        "duplicadas con sufijos .x / .y: ",
        paste(utils::head(dup, 8), collapse = ", "),
        if (length(dup) > 8) ", ..." else ""))
    })

    # Verificacion de tipos de las claves
    join_check <- shiny::reactive({
      rdf <- join_right()
      by  <- join_by()
      if (is.null(rdf) || length(by) == 0) return(NULL)
      join_key_check(join_left(), rdf, by)
    })

    output$join_check_ui <- shiny::renderUI({
      chk <- join_check()
      if (is.null(chk) || length(chk$messages) == 0) return(NULL)
      shiny::tags$div(
        class = paste("alert py-2 px-3 mb-2",
                      if (chk$ok) "alert-info" else "alert-danger"),
        lapply(chk$messages, function(m) shiny::tags$div(shiny::tags$small(m)))
      )
    })

    rv_join <- shiny::reactiveValues(n_cols = 1)

    shiny::observeEvent(input$join_add_col, {
      rv_join$n_cols <- rv_join$n_cols + 1
    })

    output$join_more_cols_ui <- shiny::renderUI({
      n <- rv_join$n_cols
      if (n <= 1) return(NULL)
      rdf <- join_right()
      right_cols <- if (is.null(rdf)) character(0) else names(rdf)
      left_cols  <- names(join_left())
      shiny::tagList(lapply(2:n, function(i) {
        sel_l <- shiny::isolate(input[[paste0("join_col_left_", i)]])
        sel_r <- shiny::isolate(input[[paste0("join_col_right_", i)]])
        shiny::fluidRow(
          shiny::column(5,
            shiny::selectInput(ns(paste0("join_col_left_", i)), NULL,
              choices = left_cols, selected = sel_l)
          ),
          shiny::column(2,
            shiny::tags$div(style = "padding-top:8px; text-align:center;",
              shiny::tags$strong("="))
          ),
          shiny::column(5,
            shiny::selectInput(ns(paste0("join_col_right_", i)), NULL,
              choices = right_cols, selected = sel_r)
          )
        )
      }))
    })

    shiny::observeEvent(input$confirm_join, {
      shiny::req(input$join_right_df)
      by_named <- join_by()
      if (length(by_named) == 0) {
        shiny::showNotification("Eleg\u00ed al menos una columna clave.", type = "warning")
        return()
      }
      chk <- join_check()
      if (!is.null(chk) && !chk$ok) {
        shiny::showNotification(paste(chk$messages, collapse = " "),
                                type = "error", duration = 10)
        return()
      }
      # NULL = todas las columnas (mismo codigo que antes de existir el selector)
      # (%||% del paquete no admite vectores de largo > 1)
      elegidas   <- input$join_right_cols
      if (is.null(elegidas)) elegidas <- character(0)
      right_cols <- if (setequal(elegidas, join_right_nonkey())) NULL else elegidas
      casts <- if (!is.null(chk) && length(chk$casts) > 0) chk$casts else NULL
      push_operation(operation_stack, history,
        op_join(input$join_right_df, by_named, input$join_type,
                right_cols   = right_cols,
                key_casts    = casts,
                right_source = rvisual_source(input$join_right_df)))
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

    # Observers para eliminar operaciones individuales
    # Usamos local() para capturar el indice correctamente en el closure
    # y evitamos once=TRUE que causaba eliminacion en cascada
    op_to_remove <- shiny::reactiveVal(NULL)

    shiny::observe({
      ops <- operation_stack()
      lapply(seq_along(ops), function(i) {
        local({
          idx <- i
          btn_id <- paste0("remove_op_", idx)
          shiny::observeEvent(input[[btn_id]], {
            op_to_remove(list(idx = idx, ts = Sys.time()))
          }, ignoreInit = TRUE)
        })
      })
    })

    shiny::observeEvent(op_to_remove(), {
      info <- op_to_remove()
      shiny::req(!is.null(info))
      current <- operation_stack()
      idx <- info$idx
      if (idx >= 1 && idx <= length(current)) {
        operation_stack(current[-idx])
      }
      op_to_remove(NULL)
    }, ignoreNULL = TRUE)

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
      src     <- if (!is.null(active_source)) active_source() else NULL
      shiny::req(df_name)
      generated_code(generate_code(df_name, ops, source = src))
    })

  })
}

# -- Helper ----------------------------------------------------------------
push_operation <- function(operation_stack, history, op) {
  operation_stack(c(shiny::isolate(operation_stack()), list(op)))
  history_log(history, "operation_added", list(type = op$type, label = op$label))
}
