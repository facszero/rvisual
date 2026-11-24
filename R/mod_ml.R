#' Modulo: Modelado (Machine Learning)
#'
#' Asistente de seis pasos: objetivo y variables, diagnostico, particion y
#' modelos, comparacion por validacion cruzada, modelo final y aplicacion
#' del modelo a datos nuevos.
#'
#' Solo LEE el estado compartido (dataset activo, su origen y las
#' operaciones del Constructor). Escribe en dos casos, siempre por accion
#' explicita del usuario: \code{generated_code} al pulsar "Enviar a Codigo
#' R", y un dataset NUEVO (nunca modifica uno existente) al pulsar
#' "Aplicar modelo" en el paso 6.
#'
#' @param id ID del modulo Shiny.
#' @return Un objeto shiny.tag con la UI del panel.
#' @keywords internal
mod_ml_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("no_data_msg")),
    bslib::navset_card_tab(
      id = ns("steps"),

      bslib::nav_panel(
        title = "1. Objetivo", value = "step1",
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::div(
            shiny::selectInput(ns("target"), "Variable objetivo", choices = NULL),
            shiny::radioButtons(ns("task"), "Tipo de tarea",
              choiceNames = unname(ml_ui_task_labels()), choiceValues = names(ml_ui_task_labels())),
            shiny::uiOutput(ns("task_hint")),
            shiny::uiOutput(ns("positive_ui"))
          ),
          shiny::div(
            shiny::selectizeInput(ns("predictors"), "Predictores", choices = NULL,
                                  multiple = TRUE,
                                  options = list(plugins = list("remove_button"))),
            shiny::div(class = "d-flex gap-2 mb-2",
              shiny::actionButton(ns("pred_all"), "Todos",
                                  class = "btn-sm btn-outline-secondary"),
              shiny::actionButton(ns("pred_none"), "Quitar todos",
                                  class = "btn-sm btn-outline-secondary")),
            shiny::uiOutput(ns("ops_notice"))
          )
        ),
        shiny::actionButton(ns("btn_diagnose"), "Diagnosticar",
                            icon = shiny::icon("stethoscope"), class = "btn-primary")
      ),

      bslib::nav_panel(
        title = "2. Diagn\u00f3stico", value = "step2",
        shiny::uiOutput(ns("diag_ui"))
      ),

      bslib::nav_panel(
        title = "3. Partici\u00f3n y modelos", value = "step3",
        shiny::uiOutput(ns("setup_ui"))
      ),

      bslib::nav_panel(
        title = "4. Comparaci\u00f3n", value = "step4",
        shiny::uiOutput(ns("results_ui"))
      ),

      bslib::nav_panel(
        title = "5. Modelo final", value = "step5",
        shiny::uiOutput(ns("final_ui"))
      ),

      bslib::nav_panel(
        title = "6. Aplicar a datos nuevos", value = "step6",
        shiny::uiOutput(ns("apply_ui")),
        shiny::uiOutput(ns("apply_result_ui"))
      )
    )
  )
}

#' Server del modulo Modelado
#'
#' @param id ID del modulo Shiny.
#' @param active_dataset reactiveVal con el data.frame activo.
#' @param active_name reactiveVal con el nombre del dataset activo.
#' @param active_source reactiveVal con los metadatos de origen (o \code{NULL}).
#' @param operation_stack reactiveVal con las operaciones del Constructor
#'   (solo lectura; o \code{NULL}).
#' @param generated_code reactiveVal del panel Codigo R (o \code{NULL}).
#' @param parent_session Sesion principal, para navegar al panel Codigo R.
#' @param add_dataset Funcion \code{function(name, data, source)} que agrega
#'   un dataset nuevo a la lista del panel Dataset (o \code{NULL}).
#' @keywords internal
mod_ml_server <- function(id, active_dataset, active_name, active_source = NULL,
                          operation_stack = NULL, generated_code = NULL,
                          parent_session = NULL, add_dataset = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    diagnosis      <- shiny::reactiveVal(NULL)
    setup_defaults <- shiny::reactiveVal(NULL)
    run_spec       <- shiny::reactiveVal(NULL)
    run_result     <- shiny::reactiveVal(NULL)
    final_result   <- shiny::reactiveVal(NULL)
    apply_result   <- shiny::reactiveVal(NULL)

    ops <- shiny::reactive({
      if (is.null(operation_stack)) list() else operation_stack()
    })
    source_meta <- shiny::reactive({
      if (is.null(active_source)) NULL else active_source()
    })
    model_data <- shiny::reactive({
      df <- active_dataset()
      shiny::req(df)
      as.data.frame(ml_apply_ops_safely(df, ops()))
    })

    reset_downstream <- function() {
      diagnosis(NULL)
      setup_defaults(NULL)
      run_spec(NULL)
      run_result(NULL)
      final_result(NULL)
      apply_result(NULL)
    }

    go_to <- function(step) bslib::nav_select("steps", step, session = session)

    # -- Paso 1: objetivo y variables --------------------------------------
    output$no_data_msg <- shiny::renderUI({
      if (is.null(active_dataset())) {
        shiny::div(class = "alert alert-info mt-3",
                   "Carg\u00e1 o seleccion\u00e1 un dataset en el panel 'Dataset'.")
      }
    })

    shiny::observeEvent(active_dataset(), reset_downstream(), ignoreNULL = FALSE)

    shiny::observe({
      cols <- names(model_data())
      current <- shiny::isolate(input$target)
      shiny::updateSelectInput(session, "target", choices = cols,
        selected = if (!is.null(current) && current %in% cols) current else character(0))
    })

    shiny::observeEvent(input$pred_all, {
      cols <- names(model_data())
      shiny::updateSelectizeInput(session, "predictors",
        selected = setdiff(cols, input$target %||% ""))
    })

    shiny::observeEvent(input$pred_none, {
      shiny::updateSelectizeInput(session, "predictors", selected = character(0))
    })

    shiny::observeEvent(input$target, {
      df <- model_data()
      shiny::req(input$target %in% names(df))
      shiny::updateSelectizeInput(session, "predictors",
        choices = setdiff(names(df), input$target),
        selected = setdiff(names(df), input$target))
      detected <- ml_detect_task(df[[input$target]])
      if (!is.na(detected)) shiny::updateRadioButtons(session, "task", selected = detected)
      reset_downstream()
    })

    output$task_hint <- shiny::renderUI({
      df <- model_data()
      shiny::req(input$target %in% names(df))
      detected <- ml_detect_task(df[[input$target]])
      txt <- if (is.na(detected)) {
        "La variable no sirve como objetivo (constante, vac\u00eda o con demasiadas clases)."
      } else {
        paste0("Detectada: ", ml_ui_task_labels()[[detected]], ".")
      }
      shiny::tags$small(class = "text-muted", txt)
    })

    output$positive_ui <- shiny::renderUI({
      df <- model_data()
      shiny::req(identical(input$task, "binary"), input$target %in% names(df))
      y <- df[[input$target]]
      counts <- table(as.character(y[!is.na(y)]))
      shiny::req(length(counts) == 2)
      shiny::selectInput(ns("positive"), "Clase positiva", choices = names(counts),
                         selected = ml_ui_default_positive(counts))
    })

    output$ops_notice <- shiny::renderUI({
      n_ops <- length(ops())
      if (n_ops > 0) {
        shiny::div(class = "alert alert-secondary small",
          shiny::icon("sliders"), " ",
          paste0("Se modela el dataset con las ", n_ops,
                 " operaciones del Constructor aplicadas (",
                 nrow(model_data()), " filas)."))
      }
    })

    shiny::observeEvent(input$btn_diagnose, {
      df <- model_data()
      if (is.null(input$target) || !nzchar(input$target) || length(input$predictors) == 0) {
        shiny::showNotification("Eleg\u00ed la variable objetivo y al menos un predictor.",
                                type = "warning")
        return()
      }
      d <- tryCatch(
        ml_diagnose(df, input$target, input$predictors, task = input$task,
                    positive = if (identical(input$task, "binary")) input$positive),
        error = function(e) {
          shiny::showNotification(paste("No se pudo diagnosticar:", conditionMessage(e)),
                                  type = "error")
          NULL
        })
      shiny::req(d)
      setup_defaults(NULL)
      run_result(NULL)
      final_result(NULL)
      diagnosis(d)
      go_to("step2")
    })

    # -- Paso 2: diagnostico ----------------------------------------------
    output$diag_ui <- shiny::renderUI({
      d <- diagnosis()
      if (is.null(d)) return(ml_ui_pending("Complet\u00e1 el paso 1 y puls\u00e1 Diagnosticar."))
      p <- d$profile
      cards <- bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        ml_ui_stat("Filas", format(p$n, big.mark = ".", decimal.mark = ",")),
        ml_ui_stat("Filas completas", format(p$n_complete, big.mark = ".", decimal.mark = ",")),
        ml_ui_stat("Predictores", nrow(p$columns)),
        ml_ui_stat("Tarea", if (is.na(d$task)) "-" else ml_ui_task_labels()[[d$task]])
      )
      findings <- lapply(d$findings, ml_ui_finding)
      sug <- d$suggested
      summary <- shiny::div(class = "small text-muted mb-2",
        shiny::strong("Sugerencias: "),
        paste0("modelos: ", paste(vapply(sug$engines, ml_ui_engine_label, ""), collapse = ", "),
               if (length(sug$exclude) > 0) paste0(" \u00b7 excluir: ", paste(sug$exclude, collapse = ", ")),
               " \u00b7 folds: ", sug$folds))
      blocked <- is.na(d$task)
      shiny::tagList(
        cards, shiny::tags$hr(), findings, summary,
        if (!blocked) shiny::div(class = "d-flex gap-2",
          shiny::actionButton(ns("btn_apply"), "Aplicar sugerencias y continuar",
                              icon = shiny::icon("check"), class = "btn-primary"),
          shiny::actionButton(ns("btn_skip"), "Continuar sin aplicar",
                              class = "btn-outline-secondary"))
      )
    })

    shiny::observeEvent(input$btn_apply, {
      d <- diagnosis()
      shiny::req(d)
      if (length(d$suggested$exclude) > 0) {
        shiny::updateSelectizeInput(session, "predictors",
          selected = setdiff(input$predictors, d$suggested$exclude))
      }
      setup_defaults(list(engines = d$suggested$engines, folds = d$suggested$folds,
                          stamp = Sys.time()))
      go_to("step3")
    })

    shiny::observeEvent(input$btn_skip, {
      d <- diagnosis()
      shiny::req(d)
      setup_defaults(list(engines = d$suggested$engines, folds = 5L, stamp = Sys.time()))
      go_to("step3")
    })

    # -- Paso 3: particion y modelos ----------------------------------------
    output$setup_ui <- shiny::renderUI({
      def <- setup_defaults()
      if (is.null(def)) return(ml_ui_pending("Complet\u00e1 el diagn\u00f3stico (paso 2)."))
      task <- shiny::isolate(input$task)
      engines <- ml_engines_for_task(task)
      choice_names <- lapply(engines, function(e) {
        def_e <- ml_engine_get(e)
        if (ml_engine_available(e)) def_e$label
        else shiny::span(def_e$label, shiny::tags$small(class = "text-danger",
                         paste0(" (falta instalar ", def_e$package, ")")))
      })
      hp_panels <- lapply(engines, function(e) {
        params <- ml_engine_get(e)$params
        if (length(params) == 0) return(NULL)
        bslib::accordion_panel(
          title = ml_ui_engine_label(e),
          lapply(names(params), function(pn) {
            pdef <- params[[pn]]
            shiny::numericInput(ns(ml_ui_hp_id(e, pn)), pdef$label %||% pn,
              value = if (is.null(pdef$default)) NA else pdef$default,
              min = pdef$min %||% NA, max = pdef$max %||% NA)
          })
        )
      })
      hp_panels <- Filter(Negate(is.null), hp_panels)

      shiny::tagList(
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::div(
            shiny::h6("Partici\u00f3n"),
            shiny::numericInput(ns("test_prop"), "Proporci\u00f3n de test", 0.25,
                                min = 0.05, max = 0.5, step = 0.05),
            shiny::numericInput(ns("seed"), "Semilla", 123L, min = 1, step = 1),
            if (task != "regression")
              shiny::checkboxInput(ns("stratify"), "Estratificar por la variable objetivo", TRUE)
          ),
          shiny::div(
            shiny::h6("Valores faltantes y validaci\u00f3n"),
            shiny::radioButtons(ns("na_action"), "Filas con NA",
              choiceNames = c("Eliminar filas incompletas",
                              "Imputar mediana / moda (dentro del entrenamiento)"),
              choiceValues = c("omit", "impute_simple")),
            shiny::numericInput(ns("folds"), "Folds de validaci\u00f3n cruzada", def$folds,
                                min = 2, max = 20, step = 1)
          ),
          shiny::div(
            shiny::h6("Modelos candidatos"),
            shiny::checkboxGroupInput(ns("models"), NULL,
              choiceNames = choice_names, choiceValues = engines,
              selected = intersect(def$engines, engines))
          )
        ),
        if (length(hp_panels) > 0)
          do.call(bslib::accordion, c(hp_panels, list(open = FALSE))),
        shiny::uiOutput(ns("setup_messages")),
        shiny::div(class = "d-flex gap-2 mt-2",
          shiny::actionButton(ns("btn_run"), "Ejecutar comparaci\u00f3n",
                              icon = shiny::icon("play"), class = "btn-success"),
          shiny::actionButton(ns("btn_preview"), "Ver script",
                              icon = shiny::icon("code"), class = "btn-outline-secondary"))
      )
    })

    current_spec <- shiny::reactive({
      shiny::req(setup_defaults(), input$target, input$task)
      models <- lapply(input$models, function(e) {
        params <- ml_engine_get(e)$params
        vals <- lapply(names(params), function(pn) {
          v <- input[[ml_ui_hp_id(e, pn)]]
          if (is.null(v) || is.na(v)) return(NULL)
          if (params[[pn]]$type == "integer") as.integer(v) else v
        })
        names(vals) <- names(params)
        ml_model(e, Filter(Negate(is.null), vals))
      })
      ml_spec(
        target     = input$target,
        predictors = input$predictors,
        task       = input$task,
        positive   = if (identical(input$task, "binary")) input$positive,
        models     = models,
        test_prop  = input$test_prop %||% 0.25,
        seed       = as.integer(input$seed %||% 123L),
        stratify   = if (identical(input$task, "regression")) FALSE else isTRUE(input$stratify),
        na_action  = input$na_action %||% "omit",
        folds      = as.integer(input$folds %||% 5L)
      )
    })

    output$setup_messages <- shiny::renderUI({
      spec <- current_spec()
      v <- ml_spec_validate(spec, data = model_data())
      notices <- ml_run_notices(model_data(), spec)
      shiny::tagList(
        lapply(v$errors, function(m) shiny::div(class = "alert alert-danger py-1 mb-1 small",
                                               shiny::icon("circle-xmark"), " ", m)),
        lapply(c(v$warnings, notices), function(m)
          shiny::div(class = "alert alert-warning py-1 mb-1 small",
                     shiny::icon("triangle-exclamation"), " ", m))
      )
    })

    shiny::observeEvent(input$btn_preview, {
      code <- ml_generate_code(current_spec(), active_name(), ops(), source_meta())
      shiny::showModal(shiny::modalDialog(
        title = "Script de modelado", size = "l", easyClose = TRUE,
        shiny::tags$pre(class = "bg-light p-2", style = "font-size:12px; max-height:60vh;", code),
        footer = shiny::modalButton("Cerrar")))
    })

    shiny::observeEvent(input$btn_run, {
      spec <- current_spec()
      spec$final <- NULL
      r <- shiny::withProgress(message = "Entrenando y validando modelos...", value = 0.3, {
        ml_run(spec, active_dataset(), df_name = active_name(), ops = ops())
      })
      run_spec(spec)
      run_result(r)
      final_result(NULL)
      if (isTRUE(r$ok)) go_to("step4")
      else shiny::showNotification(r$error, type = "error", duration = 10)
    })

    # -- Paso 4: comparacion -------------------------------------------------
    output$results_ui <- shiny::renderUI({
      r <- run_result()
      if (is.null(r)) return(ml_ui_pending("Ejecut\u00e1 la comparaci\u00f3n en el paso 3."))
      if (!isTRUE(r$ok)) return(shiny::div(class = "alert alert-danger", r$error))
      spec <- run_spec()
      candidates <- setdiff(r$comparison$modelo, "baseline")
      shiny::tagList(
        shiny::p(class = "small text-muted", paste0(
          "Entrenamiento: ", r$n_train, " filas \u00b7 test reservado: ", r$n_test,
          " filas (no se usa hasta el paso 5) \u00b7 ", spec$cv$folds, " folds \u00b7 ",
          format(round(r$elapsed, 1), decimal.mark = ","), " s")),
        DT::DTOutput(ns("comparison_table")),
        shiny::plotOutput(ns("folds_plot"), height = "260px"),
        if (length(r$warnings) > 0) shiny::tags$details(
          shiny::tags$summary(class = "small", paste0("Avisos de R (", length(r$warnings), ")")),
          shiny::tags$pre(class = "small", paste(r$warnings, collapse = "\n"))),
        shiny::hr(),
        shiny::radioButtons(ns("final_model"), "Modelo final (elecci\u00f3n del analista)",
          choiceNames = vapply(candidates, ml_ui_engine_label, "", USE.NAMES = FALSE),
          choiceValues = candidates, selected = character(0), inline = TRUE),
        shiny::actionButton(ns("btn_final"), "Entrenar modelo final y evaluar en test",
                            icon = shiny::icon("flag-checkered"), class = "btn-primary")
      )
    })

    output$comparison_table <- DT::renderDT({
      r <- run_result()
      shiny::req(r$ok)
      tab <- r$comparison
      tab$modelo <- vapply(tab$modelo, ml_ui_engine_label, "", USE.NAMES = FALSE)
      metric_cols <- setdiff(names(tab), "modelo")
      names(tab) <- c("Modelo", ml_ui_metric_labels(metric_cols))
      DT::formatRound(
        DT::datatable(tab, rownames = FALSE,
                      options = list(dom = "t", ordering = FALSE, pageLength = 20),
                      class = "table table-sm"),
        columns = seq_along(metric_cols) + 1, digits = 3, mark = ".", dec.mark = ",")
    })

    output$folds_plot <- shiny::renderPlot({
      r <- run_result()
      shiny::req(r$ok)
      metric <- ml_metric_columns(run_spec()$metrics)[1]
      res <- r$cv_results
      res$modelo <- factor(res$modelo, levels = r$comparison$modelo,
                           labels = vapply(r$comparison$modelo, ml_ui_engine_label, "",
                                           USE.NAMES = FALSE))
      ticks <- pretty(res[[metric]])
      tick_labels <- ml_ui_format_number(ticks)
      left <- max(4, 1.5 + max(nchar(tick_labels)) * 0.55)
      graphics::par(mar = c(4, left, 2, 1))
      graphics::stripchart(res[[metric]] ~ res$modelo, vertical = TRUE, method = "jitter",
                           pch = 19, col = "#2C6FAC80", yaxt = "n", ylim = range(ticks),
                           ylab = "", main = paste0(ml_ui_metric_labels(metric), " por fold"))
      graphics::axis(2, at = ticks, labels = tick_labels, las = 1)
      graphics::mtext(ml_ui_metric_labels(metric), side = 2, line = left - 1.2)
      graphics::points(seq_along(levels(res$modelo)),
                       tapply(res[[metric]], res$modelo, mean), pch = 3, cex = 2, lwd = 2)
    })

    shiny::observeEvent(input$btn_final, {
      if (is.null(input$final_model) || !nzchar(input$final_model)) {
        shiny::showNotification("Eleg\u00ed el modelo final.", type = "warning")
        return()
      }
      spec <- run_spec()
      spec$final <- input$final_model
      r <- shiny::withProgress(message = "Entrenando el modelo final...", value = 0.3, {
        ml_run(spec, active_dataset(), df_name = active_name(), ops = ops())
      })
      if (isTRUE(r$ok)) {
        final_result(list(spec = spec, result = r))
        apply_result(NULL)
        go_to("step5")
      } else {
        shiny::showNotification(r$error, type = "error", duration = 10)
      }
    })

    # -- Paso 5: modelo final ----------------------------------------------------
    final_code <- shiny::reactive({
      fr <- final_result()
      shiny::req(fr)
      ml_generate_code(fr$spec, active_name(), ops(), source_meta())
    })

    output$final_ui <- shiny::renderUI({
      fr <- final_result()
      if (is.null(fr)) return(ml_ui_pending("Eleg\u00ed y entren\u00e1 el modelo final en el paso 4."))
      task <- fr$spec$task
      shiny::tagList(
        shiny::h5(paste0("Modelo final: ", ml_ui_engine_label(fr$spec$final))),
        shiny::p(class = "small text-muted",
                 "M\u00e9tricas sobre el conjunto de test, evaluado una sola vez despu\u00e9s de elegir."),
        shiny::tableOutput(ns("final_metrics")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          if (task == "regression") shiny::plotOutput(ns("obs_pred_plot"), height = "280px")
          else shiny::div(shiny::h6("Matriz de confusi\u00f3n"), shiny::tableOutput(ns("confusion"))),
          shiny::plotOutput(ns("importance_plot"), height = "280px")
        ),
        if (task == "binary") shiny::plotOutput(ns("roc_plot"), height = "280px"),
        shiny::div(class = "d-flex gap-2 flex-wrap mt-2",
          shiny::actionButton(ns("btn_send_code"), "Enviar a C\u00f3digo R",
                              icon = shiny::icon("code"), class = "btn-primary"),
          shiny::actionButton(ns("btn_console"), "Ejecutar en consola",
                              icon = shiny::icon("terminal"), class = "btn-outline-success"),
          shiny::downloadButton(ns("dl_model"), "Guardar modelo (.rds)",
                                class = "btn-outline-primary"),
          shiny::downloadButton(ns("dl_script"), "Guardar script (.R)",
                                class = "btn-outline-primary"),
          shiny::actionButton(ns("btn_go_apply"), "Aplicar a datos nuevos",
                              icon = shiny::icon("arrow-right"),
                              class = "btn-outline-secondary"))
      )
    })

    shiny::observeEvent(input$btn_go_apply, go_to("step6"))

    output$final_metrics <- shiny::renderTable({
      fr <- final_result()
      shiny::req(fr)
      m <- fr$result$final$metrics
      m[] <- lapply(m, function(v) format(round(v, 3), nsmall = 3, big.mark = ".",
                                          decimal.mark = ","))
      names(m) <- ml_ui_metric_labels(names(m))
      m
    })

    output$confusion <- shiny::renderTable({
      fr <- final_result()
      shiny::req(fr, !is.null(fr$result$final$confusion))
      tab <- fr$result$final$confusion
      out <- as.data.frame.matrix(tab)
      out <- cbind(data.frame(x = rownames(out), stringsAsFactors = FALSE), out)
      names(out)[1] <- "Observado / Predicho"
      out
    })

    output$obs_pred_plot <- shiny::renderPlot({
      fr <- final_result()
      shiny::req(fr)
      f <- fr$result$final
      yt <- pretty(f$predicted)
      xt <- pretty(f$observed)
      left <- max(4, 1.5 + max(nchar(ml_ui_format_number(yt))) * 0.55)
      graphics::par(mar = c(4, left, 2, 1))
      graphics::plot(f$observed, f$predicted, pch = 19, col = "#2C6FAC80",
                     xaxt = "n", yaxt = "n", xlab = "Observado", ylab = "",
                     main = "Observado vs. predicho (test)")
      graphics::axis(1, at = xt, labels = ml_ui_format_number(xt))
      graphics::axis(2, at = yt, labels = ml_ui_format_number(yt), las = 1)
      graphics::mtext("Predicho", side = 2, line = left - 1.2)
      graphics::abline(0, 1, lty = 2)
    })

    output$roc_plot <- shiny::renderPlot({
      fr <- final_result()
      shiny::req(fr, identical(fr$spec$task, "binary"))
      f <- fr$result$final
      lev <- levels(f$observed)
      roc <- pROC::roc(f$observed, f$predicted, levels = lev, direction = "<", quiet = TRUE)
      graphics::par(mar = c(4, 4, 2, 1))
      graphics::plot(1 - roc$specificities, roc$sensitivities, type = "l", lwd = 2,
                     col = "#2C6FAC", las = 1, xlab = "1 - especificidad",
                     ylab = "Sensibilidad",
                     main = paste0("Curva ROC (AUC = ",
                                   format(round(as.numeric(roc$auc), 3), decimal.mark = ","), ")"))
      graphics::abline(0, 1, lty = 2, col = "grey60")
    })

    output$importance_plot <- shiny::renderPlot({
      fr <- final_result()
      shiny::req(fr)
      imp <- fr$result$final$importance
      if (is.null(imp)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Importancia de variables no disponible para este modelo.")
        return(invisible())
      }
      imp <- utils::head(imp, 15)
      graphics::par(mar = c(4, 9, 2, 1))
      graphics::barplot(rev(imp$importance), names.arg = rev(imp$variable), horiz = TRUE,
                        las = 1, col = "#2C6FAC", border = NA, xlim = c(0, 100),
                        xlab = "Importancia relativa (m\u00e1ximo = 100)",
                        main = "Importancia de variables")
    })

    shiny::observeEvent(input$btn_send_code_apply, {
      shiny::req(generated_code)
      generated_code(final_code())
      if (!is.null(parent_session)) {
        bslib::nav_select("main_nav", "panel_code", session = parent_session)
      }
      shiny::showNotification("Script de modelado enviado al panel C\u00f3digo R.",
                              type = "message", duration = 3)
    })

    shiny::observeEvent(input$btn_send_code, {
      shiny::req(generated_code)
      generated_code(final_code())
      if (!is.null(parent_session)) {
        bslib::nav_select("main_nav", "panel_code", session = parent_session)
      }
      shiny::showNotification("Script de modelado enviado al panel C\u00f3digo R.",
                              type = "message", duration = 3)
    })

    shiny::observeEvent(input$btn_console, {
      shiny::showModal(shiny::modalDialog(
        title = "Ejecutar en la consola de R",
        shiny::p("El script se ejecutar\u00e1 en tu sesi\u00f3n de R: crea objetos en el entorno global y guarda el modelo en el directorio de trabajo."),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_console"), "Ejecutar", class = "btn-success"))))
    })

    shiny::observeEvent(input$confirm_console, {
      shiny::removeModal()
      tryCatch(rstudio_run_in_console(final_code()),
               error = function(e) shiny::showNotification(
                 paste("No se pudo ejecutar:", conditionMessage(e)), type = "error"))
    })

    output$dl_model <- shiny::downloadHandler(
      filename = function() paste0("modelo_", final_result()$spec$final, ".rds"),
      content = function(file) saveRDS(final_result()$result$final$fit, file)
    )

    output$dl_script <- shiny::downloadHandler(
      filename = function() paste0("modelado_", active_name() %||% "rvisual", "_",
                                   format(Sys.time(), "%Y%m%d_%H%M"), ".R"),
      content = function(file) writeLines(final_code(), file),
      contentType = "text/plain"
    )

    # -- Paso 6: aplicar el modelo a datos nuevos ----------------------------
    output$apply_ui <- shiny::renderUI({
      fr <- final_result()
      if (is.null(fr)) return(ml_ui_pending("Entren\u00e1 el modelo final en el paso 4."))
      nm  <- shiny::isolate(active_name())
      cur <- shiny::isolate(ops())
      prev <- fr$spec$apply
      default_idx <- if (!is.null(prev)) prev$ops_idx
                     else which(vapply(cur, function(o) !identical(o$type, "filter"), logical(1)))
      shiny::tagList(
        shiny::p(class = "small text-muted",
          "Predice con el modelo final sobre otro conjunto de filas y lo guarda como un ",
          "dataset nuevo en la pesta\u00f1a Dataset. Los datos se preparan igual que en el ",
          "entrenamiento: una categor\u00eda que el modelo no vio, o un NA sin imputar, ",
          "deja la fila sin predicci\u00f3n."),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::div(
            shiny::selectInput(ns("apply_dataset"), "Dataset sobre el que predecir",
                               choices = rvisual_join_candidates(),
                               selected = prev$dataset %||% nm),
            if (length(cur) > 0) shiny::checkboxGroupInput(ns("apply_ops"),
              "Operaciones del Constructor a aplicar antes",
              choiceNames = vapply(cur, function(o) as.character(o$label), "", USE.NAMES = FALSE),
              choiceValues = seq_along(cur), selected = default_idx),
            if (length(cur) > 0) shiny::p(class = "small text-muted",
              "Por defecto se aplican todas menos los filtros: los filtros eligen las filas ",
              "del entrenamiento y aqu\u00ed se eligen otras.")
          ),
          shiny::div(
            shiny::tags$label(class = "form-label", "Filtro de las filas a predecir (opcional)"),
            shiny::div(class = "d-flex gap-2",
              shiny::selectInput(ns("apply_f_col"), NULL, choices = c("(sin filtro)" = ""),
                                 width = "45%"),
              shiny::selectInput(ns("apply_f_op"), NULL,
                                 choices = c("==", "!=", ">", ">=", "<", "<="),
                                 selected = prev$filter$params$op %||% "==", width = "20%"),
              shiny::textInput(ns("apply_f_val"), NULL,
                               value = prev$filter$params$value %||% "", width = "35%")),
            shiny::textInput(ns("apply_output"), "Nombre del dataset resultado",
                             value = prev$output %||% paste0(nm, "_pred")),
            shiny::uiOutput(ns("apply_count"))
          )
        ),
        shiny::actionButton(ns("btn_apply_model"), "Aplicar modelo",
                            icon = shiny::icon("wand-magic-sparkles"), class = "btn-primary")
      )
    })

    # Dataset destino con las operaciones elegidas (sin el filtro)
    apply_base <- shiny::reactive({
      shiny::req(final_result(), input$apply_dataset)
      df <- if (identical(input$apply_dataset, active_name())) active_dataset()
            else tryCatch(rvisual_find_dataset(input$apply_dataset), error = function(e) NULL)
      shiny::req(df)
      idx <- as.integer(input$apply_ops)
      sel <- ops()[intersect(idx, seq_along(ops()))]
      tryCatch(as.data.frame(apply_operations(df, sel)),
               error = function(e) structure(conditionMessage(e), class = "ml_apply_error"))
    })

    shiny::observe({
      base <- apply_base()
      cols <- if (is.data.frame(base)) names(base) else character(0)
      prev <- shiny::isolate(input$apply_f_col)
      if (is.null(prev) || !nzchar(prev)) {
        fr <- shiny::isolate(final_result())
        prev <- fr$spec$apply$filter$params$col %||% ""
      }
      shiny::updateSelectInput(session, "apply_f_col",
        choices = c("(sin filtro)" = "", cols),
        selected = if (prev %in% cols) prev else "")
    })

    apply_filter_op <- shiny::reactive({
      col <- input$apply_f_col %||% ""
      val <- trimws(input$apply_f_val %||% "")
      if (!nzchar(col) || !nzchar(val)) return(NULL)
      base <- apply_base()
      numeric <- if (is.data.frame(base) && col %in% names(base)) is.numeric(base[[col]]) else NULL
      op_filter(col, input$apply_f_op, val, numeric = numeric)
    })

    output$apply_count <- shiny::renderUI({
      base <- apply_base()
      if (!is.data.frame(base)) {
        return(shiny::div(class = "alert alert-danger py-1 small",
          "Las operaciones elegidas no se pueden aplicar a este dataset: ", unclass(base)))
      }
      f <- apply_filter_op()
      n <- tryCatch(nrow(if (is.null(f)) base else apply_operations(base, list(f))),
                    error = function(e) NA_integer_)
      falta <- setdiff(final_result()$spec$predictors, names(base))
      shiny::tagList(
        shiny::p(class = "small mb-1", if (is.na(n)) "El filtro no se puede aplicar."
                 else paste0("Filas a predecir: ", ml_ui_format_number(n))),
        if (length(falta) > 0) shiny::div(class = "alert alert-danger py-1 small",
          "Faltan predictores en este dataset: ", paste(falta, collapse = ", "))
      )
    })

    shiny::observeEvent(input$btn_apply_model, {
      fr <- final_result()
      shiny::req(fr)
      dest <- input$apply_dataset
      out_name <- trimws(input$apply_output %||% "")
      if (!nzchar(out_name) || !identical(make.names(out_name), out_name)) {
        shiny::showNotification(paste0(
          "El nombre del resultado tiene que ser un nombre v\u00e1lido de R ",
          "(letras, n\u00fameros, punto y gui\u00f3n bajo; sin espacios)."),
          type = "error", duration = 8)
        return()
      }
      src_out <- rvisual_source(out_name)
      if (out_name %in% c(active_name(), dest) ||
          (exists(out_name, envir = rvisual_env(), inherits = FALSE) &&
           !identical(src_out$kind, "modelo_ml"))) {
        shiny::showNotification(paste0(
          "Ya hay un dataset llamado '", out_name, "'. Eleg\u00ed otro nombre."),
          type = "error", duration = 8)
        return()
      }
      spec <- fr$spec
      spec$apply <- ml_apply_spec(
        dest, ops_idx = as.integer(input$apply_ops), filter = apply_filter_op(),
        output = out_name,
        source = if (identical(dest, active_name())) NULL else rvisual_source(dest))
      a <- shiny::withProgress(message = "Aplicando el modelo...", value = 0.4, {
        ml_run_apply(fr$result, spec, active_name(), ops())
      })
      if (!isTRUE(a$ok)) {
        shiny::showNotification(a$error, type = "error", duration = 10)
        return()
      }
      fr$spec <- spec          # el script entregado incluye la seccion 9
      final_result(fr)
      apply_result(a)
      if (!is.null(add_dataset)) {
        add_dataset(out_name, a$data, list(kind = "modelo_ml"))
      }
    })

    output$apply_result_ui <- shiny::renderUI({
      a <- apply_result()
      if (is.null(a)) return(NULL)
      unseen <- a$unseen[a$unseen > 0]
      sin_pred <- a$n - a$n_pred
      out_name <- final_result()$spec$apply$output
      shiny::tagList(
        shiny::hr(),
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          ml_ui_stat("Filas", ml_ui_format_number(a$n)),
          ml_ui_stat("Con predicci\u00f3n", ml_ui_format_number(a$n_pred)),
          ml_ui_stat("Sin predicci\u00f3n", ml_ui_format_number(sin_pred)),
          ml_ui_stat("Tiempo", paste0(format(round(a$elapsed, 1), decimal.mark = ","), " s"))
        ),
        if (length(unseen) > 0) shiny::div(class = "alert alert-warning py-2 mt-2 small",
          "Categor\u00edas que el modelo no vio en el entrenamiento: ",
          paste0(names(unseen), " (", ml_ui_format_number(unseen), " filas)", collapse = ", "),
          "."),
        shiny::div(class = "alert alert-success py-2 mt-2 small",
          shiny::icon("circle-check"), " Se agreg\u00f3 el dataset ", shiny::strong(out_name),
          " en la pesta\u00f1a Dataset, con las columnas ",
          paste(a$columns, collapse = ", "), ". El script del modelado ahora incluye ",
          "esta aplicaci\u00f3n (secci\u00f3n 9)."),
        DT::DTOutput(ns("apply_table")),
        shiny::div(class = "d-flex gap-2 flex-wrap mt-2",
          shiny::actionButton(ns("btn_send_code_apply"), "Enviar a C\u00f3digo R",
                              icon = shiny::icon("code"), class = "btn-primary"))
      )
    })

    output$apply_table <- DT::renderDT({
      a <- apply_result()
      shiny::req(a)
      d <- utils::head(a$data, 200)
      d <- d[, c(a$columns, setdiff(names(d), a$columns)), drop = FALSE]
      # Solo las columnas agregadas: el resto se muestra como en el dataset
      num <- which(names(d) %in% a$columns & vapply(d, is.double, logical(1)))
      tab <- DT::datatable(d, rownames = FALSE, class = "table table-sm",
                           options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
      if (length(num) > 0)
        tab <- DT::formatRound(tab, columns = num, digits = 4, mark = ".", dec.mark = ",")
      tab
    })

    invisible(list(diagnosis = diagnosis, run_result = run_result,
                   final_result = final_result, apply_result = apply_result))
  })
}

# -- Ayudantes de interfaz ----------------------------------------------------

#' Formatear numeros para ejes y tablas (punto de miles, coma decimal)
#' @param x Vector numerico.
#' @return Character vector.
#' @keywords internal
ml_ui_format_number <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Etiquetas de tarea para la interfaz
#' @return Vector nombrado.
#' @keywords internal
ml_ui_task_labels <- function() {
  c(regression = "Regresi\u00f3n (objetivo num\u00e9rico)",
    binary     = "Clasificaci\u00f3n binaria",
    multiclass = "Clasificaci\u00f3n multiclase")
}

#' Clase positiva sugerida
#'
#' Prefiere valores afirmativos habituales (Si, Yes, 1, TRUE, S); si no hay
#' ninguno, la clase minoritaria.
#'
#' @param counts Tabla de frecuencias de la variable objetivo.
#' @return Nombre de la clase.
#' @keywords internal
ml_ui_default_positive <- function(counts) {
  affirmative <- c("si", "s\u00ed", "yes", "1", "true", "s", "y")
  hit <- names(counts)[tolower(names(counts)) %in% affirmative]
  if (length(hit) > 0) return(hit[1])
  names(which.min(counts))
}

#' Etiquetas de metricas para la interfaz
#' @param cols Nombres de columna de metricas en el script.
#' @return Character vector.
#' @keywords internal
ml_ui_metric_labels <- function(cols) {
  map <- c(rmse = "RMSE", mae = "MAE", r2 = "R\u00b2", auc = "AUC", f1_macro = "F1-macro",
           exactitud = "Exactitud", sensibilidad = "Sensibilidad",
           especificidad = "Especificidad")
  out <- unname(map[cols])
  ifelse(is.na(out), cols, out)
}

#' Etiqueta corta de un motor
#' @param engine Identificador.
#' @return String.
#' @keywords internal
ml_ui_engine_label <- function(engine) {
  if (identical(engine, "baseline")) return("L\u00ednea base")
  ml_engine_get(engine)$label
}

#' Id de input de un hiperparametro
#' @param engine Motor.
#' @param param Hiperparametro.
#' @return String sin puntos (seguro como id de Shiny).
#' @keywords internal
ml_ui_hp_id <- function(engine, param) {
  paste0("hp_", engine, "_", gsub(".", "_", param, fixed = TRUE))
}

ml_ui_pending <- function(msg) {
  shiny::div(class = "alert alert-light text-muted mt-2", shiny::icon("circle-info"), " ", msg)
}

ml_ui_stat <- function(label, value) {
  shiny::div(class = "border rounded p-2 text-center",
    shiny::div(class = "small text-muted", label),
    shiny::div(class = "fs-5 fw-semibold", value))
}

ml_ui_finding <- function(f) {
  cls <- switch(f$severity, critical = "alert-danger", warning = "alert-warning", "alert-info")
  icon <- switch(f$severity, critical = "circle-xmark", warning = "triangle-exclamation",
                 "circle-info")
  shiny::div(class = paste("alert py-2 mb-2", cls),
    shiny::icon(icon), " ", f$message,
    if (nzchar(f$evidence)) shiny::div(class = "small text-muted mt-1", f$evidence))
}
