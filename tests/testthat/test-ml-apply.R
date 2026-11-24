# Tests de "Aplicar el modelo a datos nuevos" (paso 6 del panel Modelado)

autos_base <- function() {
  d <- mtcars
  d$id <- seq_len(nrow(d))
  d$tipo <- ifelse(d$am == 1, "manual", "auto")
  d$entrenar <- rep(c(1L, 1L, 0L), length.out = nrow(d))
  d
}

spec_reg <- function(na_action = "omit") {
  s <- ml_spec("mpg", c("wt", "hp", "tipo"), task = "regression",
               models = list(ml_model("baseline"), ml_model("lm")), folds = 3L,
               na_action = na_action)
  s$final <- "lm"
  s
}

test_that("ml_apply_spec valida y arma la especificacion", {
  ap <- ml_apply_spec("autos", ops_idx = c(2, 1), filter = op_filter("entrenar", "==", "0"))
  expect_identical(ap$ops_idx, c(2L, 1L))
  expect_identical(ap$output, "autos_pred")
  expect_error(ml_apply_spec(""))
})

test_that("sin apply (o sin modelo final) el script no cambia", {
  s <- spec_reg()
  expect_identical(ml_code_apply(s, "autos"), "")
  s$apply <- ml_apply_spec("autos")
  s$final <- NULL
  expect_identical(ml_code_apply(s, "autos"), "")
})

test_that("pantalla = script entregado al aplicar el modelo (regresion)", {
  autos <- autos_base()
  ops <- list(op_filter("entrenar", "==", "1"))
  s <- spec_reg()
  r <- ml_run(s, autos, df_name = "autos", ops = ops)
  expect_true(r$ok)
  expect_true(is.environment(r$env))

  s$apply <- ml_apply_spec("autos", filter = op_filter("entrenar", "==", "0"),
                           output = "autos_pred")
  a <- ml_run_apply(r, s, "autos", ops)
  expect_true(a$ok)
  expect_identical(a$columns, "pred_mpg")
  expect_identical(a$n, sum(autos$entrenar == 0L))
  expect_identical(a$n_pred, a$n)
  expect_equal(a$data$pred_mpg,
               unname(stats::predict(r$final$fit, a$data)))

  ruta <- tempfile(fileext = ".rds")
  saveRDS(autos, ruta)
  script <- ml_generate_code(s, "autos", ops = ops,
                             source = list(kind = "archivo", ext = "rds", filename = ruta))
  expect_match(script, "9. Aplicar el modelo a datos nuevos", fixed = TRUE)
  env <- new.env(parent = globalenv())
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  utils::capture.output(eval(parse(text = script), envir = env))
  expect_equal(env$autos_pred, a$data)
})

test_that("categoria nueva y NA: sin prediccion, o imputados con la referencia", {
  autos <- autos_base()
  nuevos <- autos[1:4, ]
  nuevos$tipo[1] <- "cvt"      # categoria que el modelo no vio
  nuevos$hp[2] <- NA
  assign("tj_nuevos", nuevos, envir = rvisual_env())
  on.exit(rm("tj_nuevos", envir = rvisual_env()))

  s <- spec_reg()
  r <- ml_run(s, autos, df_name = "autos")
  s$apply <- ml_apply_spec("tj_nuevos", output = "res")
  a <- ml_run_apply(r, s, "autos")
  expect_true(a$ok)
  expect_true(all(is.na(a$data$pred_mpg[1:2])))
  expect_false(anyNA(a$data$pred_mpg[3:4]))
  expect_identical(a$n_pred, 2L)
  expect_identical(a$unseen[["tipo"]], 1L)

  # con imputacion simple el NA numerico se completa; la categoria nueva
  # pasa a la moda del entrenamiento
  s2 <- spec_reg("impute_simple")
  r2 <- ml_run(s2, autos, df_name = "autos")
  s2$apply <- ml_apply_spec("tj_nuevos", output = "res")
  a2 <- ml_run_apply(r2, s2, "autos")
  expect_false(anyNA(a2$data$pred_mpg))
})

test_that("clasificacion binaria y multiclase agregan probabilidades y clase", {
  autos <- autos_base()
  autos$am_f <- ifelse(autos$am == 1, "si", "no")
  sb <- ml_spec("am_f", c("wt", "hp"), task = "binary", positive = "si",
                models = list(ml_model("baseline"), ml_model("glm")), folds = 3L)
  sb$final <- "glm"
  rb <- suppressWarnings(ml_run(sb, autos, df_name = "autos"))
  sb$apply <- ml_apply_spec("autos", output = "res")
  ab <- ml_run_apply(rb, sb, "autos")
  expect_true(ab$ok)
  expect_identical(ab$columns, c("prob_si", "clase_pred"))
  expect_true(all(ab$data$prob_si >= 0 & ab$data$prob_si <= 1))
  expect_setequal(unique(ab$data$clase_pred), c("si", "no"))

  autos$cyl_f <- paste0("c", autos$cyl)
  sm <- ml_spec("cyl_f", c("wt", "hp"), task = "multiclass",
                models = list(ml_model("baseline"), ml_model("rpart")), folds = 3L)
  sm$final <- "rpart"
  rm_ <- ml_run(sm, autos, df_name = "autos")
  sm$apply <- ml_apply_spec("autos", output = "res")
  am_ <- ml_run_apply(rm_, sm, "autos")
  expect_true(am_$ok)
  expect_identical(am_$columns, c("prob_c4", "prob_c6", "prob_c8", "clase_pred"))
  expect_equal(unname(rowSums(am_$data[, c("prob_c4", "prob_c6", "prob_c8")])),
               rep(1, nrow(autos)))
})

test_that("ml_run_apply informa si no hay modelo final", {
  s <- spec_reg()
  s$apply <- ml_apply_spec("autos")
  a <- ml_run_apply(list(), s, "autos")
  expect_false(a$ok)
  expect_match(a$error, "modelo final")
})

test_that("el paso 6 del modulo crea un dataset nuevo sin tocar el activo", {
  autos <- autos_base()
  assign("autos", autos, envir = rvisual_env())
  on.exit(rm("autos", envir = rvisual_env()))
  ops <- list(op_filter("entrenar", "==", "1"))
  s <- spec_reg()
  r <- ml_run(s, autos, df_name = "autos", ops = ops)

  ds <- shiny::reactiveVal(autos)
  nm <- shiny::reactiveVal("autos")
  st <- shiny::reactiveVal(ops)
  recibido <- NULL
  shiny::testServer(mod_ml_server,
    args = list(active_dataset = ds, active_name = nm, operation_stack = st,
                add_dataset = function(name, data, source)
                  recibido <<- list(name = name, data = data, source = source)), {
      session$flushReact()   # deja correr el reinicio inicial del asistente
      final_result(list(spec = s, result = r))
      session$setInputs(apply_dataset = "autos", apply_ops = character(0),
                        apply_f_col = "entrenar", apply_f_op = "==",
                        apply_f_val = "0", apply_output = "autos pred")
      expect_true(is.data.frame(apply_base()))
      # nombre invalido: no se aplica
      session$setInputs(btn_apply_model = 1)
      expect_null(apply_result())
      # nombre igual al dataset activo: no se aplica
      session$setInputs(apply_output = "autos", btn_apply_model = 2)
      expect_null(apply_result())

      session$setInputs(apply_output = "autos_pred", btn_apply_model = 3)
      a <- apply_result()
      expect_true(a$ok)
      expect_identical(a$n, sum(autos$entrenar == 0L))
      expect_identical(recibido$name, "autos_pred")
      expect_identical(recibido$source$kind, "modelo_ml")
      expect_true("pred_mpg" %in% names(recibido$data))
      # el script del paso 5 incluye ahora la seccion 9
      expect_match(final_code(), "9. Aplicar el modelo a datos nuevos", fixed = TRUE)
      expect_match(final_code(), "dplyr::filter(entrenar == 0)", fixed = TRUE)
      # el dataset activo no cambio
      expect_identical(ds(), autos)
    })
})

test_that("el panel Dataset recibe datasets creados por otros modulos", {
  ds <- shiny::reactiveVal(NULL)
  nm <- shiny::reactiveVal(NULL)
  hist <- shiny::reactiveVal(list())
  inc <- shiny::reactiveVal(NULL)
  on.exit(if (exists("tj_creado", envir = rvisual_env(), inherits = FALSE))
    rm("tj_creado", envir = rvisual_env()))
  shiny::testServer(mod_dataset_server,
    args = list(active_dataset = ds, active_name = nm, history = hist, incoming = inc), {
      inc(list(name = "tj_creado", data = mtcars, source = list(kind = "modelo_ml")))
      session$flushReact()
      expect_identical(rv$datasets$tj_creado, mtcars)
      expect_identical(rv$sources$tj_creado$kind, "modelo_ml")
      expect_null(ds())    # no cambia el dataset activo
      expect_identical(rvisual_source("tj_creado")$kind, "modelo_ml")
    })
  expect_match(generate_load_step("tj_creado", list(kind = "modelo_ml")),
               "Modelado (ML)", fixed = TRUE)
})
