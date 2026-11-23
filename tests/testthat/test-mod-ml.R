# Tests del panel Modelado (incremento 5): logica del servidor con
# shiny::testServer, sin navegador.

ech_ej <- function() {
  ruta <- system.file("extdata", "ech_sintetica.rds", package = "rvisual")
  if (!nzchar(ruta)) skip("No se encuentra el dataset de ejemplo")
  readRDS(ruta)
}

ml_args_test <- function(data, ops = list()) {
  list(
    active_dataset  = shiny::reactiveVal(data),
    active_name     = shiny::reactiveVal("ech"),
    active_source   = shiny::reactiveVal(list(kind = "archivo", ext = "rds",
                                              filename = "ech_sintetica.rds")),
    operation_stack = shiny::reactiveVal(ops),
    generated_code  = shiny::reactiveVal("codigo previo")
  )
}

test_that("la UI del panel se construye y la app principal lo incluye", {
  ui <- mod_ml_ui("ml")
  html <- as.character(ui)
  for (step in paste0("step", 1:5)) expect_match(html, step, fixed = TRUE)
  app_html <- as.character(rvisual_ui())
  expect_match(app_html, "panel_ml", fixed = TRUE)
  for (p in c("panel_dataset", "panel_explorer", "panel_builder", "panel_code",
              "panel_ai", "panel_config")) {
    expect_match(app_html, p, fixed = TRUE)
  }
})

test_that("flujo completo binario: diagnostico, comparacion, final y envio", {
  skip_if_not_installed("pROC")
  args <- ml_args_test(ech_ej())
  shiny::testServer(mod_ml_server, args = args, {
    session$setInputs(target = "ocupado", task = "binary", positive = "Si",
                      predictors = c("edad", "sexo", "anios_educ", "id_persona"))
    expect_null(diagnosis())

    session$setInputs(btn_diagnose = 1)
    expect_identical(diagnosis()$task, "binary")
    expect_identical(diagnosis()$suggested$exclude, "id_persona")
    expect_match(as.character(output$diag_ui$html), "identificador")

    session$setInputs(btn_apply = 1)
    expect_false(is.null(setup_defaults()))
    expect_match(as.character(output$setup_ui$html), "Modelos candidatos")

    # Lo que haria el usuario en el paso 3 (la exclusion ya aplicada)
    session$setInputs(predictors = c("edad", "sexo", "anios_educ"),
                      models = c("baseline", "glm", "rpart"),
                      test_prop = 0.25, seed = 123, stratify = TRUE,
                      na_action = "omit", folds = 3)
    expect_true(ml_spec_validate(current_spec(), data = ech_ej())$valid)

    session$setInputs(btn_run = 1)
    r <- run_result()
    expect_true(r$ok)
    expect_setequal(r$comparison$modelo, c("baseline", "glm", "rpart"))
    expect_match(as.character(output$results_ui$html), "Modelo final")
    expect_false(is.null(output$comparison_table))
    expect_false(is.null(output$folds_plot))
    expect_null(final_result())

    session$setInputs(final_model = "glm", btn_final = 1)
    fr <- final_result()
    expect_identical(fr$spec$final, "glm")
    expect_identical(sum(fr$result$final$confusion), r$n_test)
    expect_match(as.character(output$final_ui$html), "Guardar modelo")
    expect_match(output$final_metrics, "AUC")
    expect_false(is.null(output$roc_plot))
    expect_false(is.null(output$importance_plot))

    # La comparacion del paso 4 y la del modelo final coinciden (misma semilla)
    expect_identical(fr$result$comparison, r$comparison)

    # Unica escritura al estado compartido: al pulsar Enviar a Codigo R
    expect_identical(generated_code(), "codigo previo")
    session$setInputs(btn_send_code = 1)
    expect_match(generated_code(), "Script generado por RVisual - Modelado", fixed = TRUE)
    expect_match(generated_code(), "ech <- readRDS(\"ech_sintetica.rds\")", fixed = TRUE)
    expect_match(generated_code(), "saveRDS(ajuste_final", fixed = TRUE)
    expect_identical(operation_stack(), list())
  })
})

test_that("regresion con un filtro del Constructor y descarga del script", {
  args <- ml_args_test(ech_ej(), ops = list(op_filter("ocupado", "==", "Si")))
  shiny::testServer(mod_ml_server, args = args, {
    session$setInputs(target = "ingreso_laboral", task = "regression",
                      predictors = c("anios_educ", "horas_trabajadas", "sexo"))
    expect_match(as.character(output$ops_notice$html), "1 operaciones del Constructor")
    session$setInputs(btn_diagnose = 1)
    expect_identical(diagnosis()$task, "regression")
    session$setInputs(btn_skip = 1)
    session$setInputs(models = c("baseline", "lm"), test_prop = 0.25, seed = 1,
                      na_action = "impute_simple", folds = 3)
    session$setInputs(btn_run = 1)
    expect_true(run_result()$ok)
    session$setInputs(final_model = "lm", btn_final = 1)
    expect_false(is.null(output$obs_pred_plot))
    script <- readLines(output$dl_script)
    expect_true(any(grepl("dplyr::filter(ocupado == \"Si\")", script, fixed = TRUE)))
    modelo <- readRDS(output$dl_model)
    expect_s3_class(modelo, "lm")
  })
})

test_that("una especificacion invalida muestra errores y no ejecuta", {
  args <- ml_args_test(ech_ej())
  shiny::testServer(mod_ml_server, args = args, {
    session$setInputs(target = "ingreso_laboral", task = "regression",
                      predictors = c("anios_educ"))
    session$setInputs(btn_diagnose = 1)
    session$setInputs(btn_skip = 1)
    session$setInputs(models = c("glm"), test_prop = 0.25, seed = 1,
                      na_action = "omit", folds = 3)
    expect_match(as.character(output$setup_messages$html), "no admite")
    session$setInputs(btn_run = 1)
    expect_false(run_result()$ok)
    expect_match(as.character(output$results_ui$html), "alert-danger")
  })
})

test_that("cambiar de dataset reinicia el asistente", {
  args <- ml_args_test(ech_ej())
  shiny::testServer(mod_ml_server, args = args, {
    session$setInputs(target = "ocupado", task = "binary", positive = "Si",
                      predictors = c("edad"))
    session$setInputs(btn_diagnose = 1)
    expect_false(is.null(diagnosis()))
    active_dataset(mtcars)
    session$flushReact()
    expect_null(diagnosis())
    expect_match(as.character(output$diag_ui$html), "paso 1")
  })
})

test_that("sin dataset el panel lo indica", {
  args <- ml_args_test(NULL)
  shiny::testServer(mod_ml_server, args = args, {
    expect_match(as.character(output$no_data_msg$html), "Dataset")
  })
})

test_that("ayudantes de interfaz", {
  expect_identical(ml_ui_default_positive(table(c("Si", "No", "No"))), "Si")
  expect_identical(ml_ui_default_positive(table(c("A", "B", "B"))), "A")
  expect_identical(ml_ui_hp_id("ranger", "num.trees"), "hp_ranger_num_trees")
  expect_identical(ml_ui_metric_labels(c("auc", "exactitud", "otra")),
                   c("AUC", "Exactitud", "otra"))
  expect_identical(ml_ui_engine_label("baseline"), "L\u00ednea base")
})
