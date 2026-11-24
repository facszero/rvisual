# Tests del generador de codigo del panel Modelado (incremento 2)

# Ejecuta un script generado en un entorno aislado donde existe el dataset
# con el nombre dado. Devuelve el entorno para inspeccionar resultados.
run_generated <- function(code, df_name, df) {
  env <- new.env(parent = globalenv())
  assign(df_name, df, envir = env)
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  utils::capture.output(eval(parse(text = code), envir = env))
  env
}

engines_installed <- function(task, exclude = "xgboost") {
  e <- setdiff(ml_engines_for_task(task), exclude)
  Filter(ml_engine_available, e)
}

datos_binarios <- function(n = 300) {
  set.seed(42)
  d <- data.frame(
    edad  = round(stats::runif(n, 18, 70)),
    sexo  = sample(c("H", "M"), n, TRUE),
    educ  = sample(c("Primaria", "Media", "Terciaria"), n, TRUE),
    stringsAsFactors = FALSE
  )
  lp <- -2 + 0.05 * d$edad + 0.8 * (d$educ == "Terciaria")
  d$ocupado <- ifelse(stats::runif(n) < stats::plogis(lp), "Si", "No")
  d
}

# -- Utilidades -------------------------------------------------------------

test_that("ml_lit formatea literales como codigo R", {
  expect_identical(ml_lit("a\"b"), "\"a\\\"b\"")
  expect_identical(ml_lit(c("x", "y")), "c(\"x\", \"y\")")
  expect_identical(ml_lit(TRUE), "TRUE")
  expect_identical(ml_lit(123L), "123L")
  expect_identical(ml_lit(0.25), "0.25")
  expect_identical(ml_lit(500), "500")
  largo <- ml_lit(paste0("variable_", 1:12))
  expect_match(largo, "^c\\(\n")
  expect_identical(eval(parse(text = largo)), paste0("variable_", 1:12))
})

test_that("ml_args omite NULL y traduce nombres", {
  expect_identical(ml_args(list(a = 1L, b = NULL, c = 0.5)), "a = 1L, c = 0.5")
  expect_identical(ml_args(list()), "")
  expect_identical(ml_args(list(eta = 0.1), c(eta = "learning_rate")), "learning_rate = 0.1")
})

test_that("ml_metric_columns traduce identificadores", {
  expect_identical(ml_metric_columns(c("auc", "accuracy")), c("auc", "exactitud"))
})

# -- Estructura del script ----------------------------------------------------

test_that("el script generado es ASCII, parsea y tiene las 8 secciones", {
  for (task in ml_tasks()) {
    spec <- ml_spec("y", c("x1", "x2"), task = task,
                    positive = if (task == "binary") "Si",
                    models = lapply(ml_engines_for_task(task), ml_model))
    code <- ml_generate_code(spec, "df", timestamp = FALSE)
    expect_false(grepl("[^\\x01-\\x7F]", code, perl = TRUE))
    expect_silent(parse(text = code))
    for (i in 1:8) expect_match(code, paste0("# -- ", i, "\\. "))
  }
})

test_that("el script es determinista sin timestamp", {
  spec <- ml_spec("mpg", c("wt", "hp"), task = "regression",
                  models = list(ml_model("baseline"), ml_model("lm")))
  expect_identical(ml_generate_code(spec, "mtcars", timestamp = FALSE),
                   ml_generate_code(spec, "mtcars", timestamp = FALSE))
})

test_that("el bloque de datos reutiliza el paso de carga y las operaciones", {
  spec <- ml_spec("mpg", c("wt", "hp"), task = "regression",
                  models = list(ml_model("baseline"), ml_model("lm")))
  code <- ml_generate_code(spec, "autos",
                           ops = list(op_filter("cyl", ">", 4)),
                           source = list(kind = "archivo", ext = "rds",
                                         filename = "autos.rds"),
                           timestamp = FALSE)
  expect_match(code, "autos <- readRDS(\"autos.rds\")", fixed = TRUE)
  expect_match(code, "datos <- autos |>\n  dplyr::filter(", fixed = TRUE)
})

test_that("los NULL de hiperparametros no se escriben", {
  spec <- ml_spec("mpg", c("wt", "hp"), task = "regression",
                  models = list(ml_model("ranger")))
  code <- ml_generate_code(spec, "mtcars", timestamp = FALSE)
  expect_false(grepl("mtry", code))
  expect_match(code, "num.trees = 500L", fixed = TRUE)
})

test_that("matriz_x solo se genera si algun modelo la usa", {
  spec <- ml_spec("mpg", c("wt", "hp"), task = "regression",
                  models = list(ml_model("baseline"), ml_model("lm")))
  expect_false(grepl("matriz_x", ml_generate_code(spec, "mtcars")))
  spec$models <- list(ml_model("baseline"), ml_model("glmnet"))
  expect_match(ml_generate_code(spec, "mtcars"), "matriz_x <- function(d)", fixed = TRUE)
})

test_that("el modelo final aparece solo si fue elegido", {
  spec <- ml_spec("mpg", c("wt", "hp"), task = "regression",
                  models = list(ml_model("baseline"), ml_model("lm")))
  expect_match(ml_generate_code(spec, "mtcars"), "modelo_elegido <- NULL", fixed = TRUE)
  spec$final <- "lm"
  expect_match(ml_generate_code(spec, "mtcars"), "modelo_elegido <- \"lm\"", fixed = TRUE)
})

test_that("un motor sin plantilla produce un error claro", {
  expect_error(ml_engine_template("svm", list(), "regression", 1L), "sin plantilla")
})

# -- Ejecucion real ------------------------------------------------------------

test_that("regresion: el script corre y compara todos los modelos", {
  engines <- engines_installed("regression")
  spec <- ml_spec("mpg", c("wt", "hp", "cyl"), task = "regression",
                  models = lapply(engines, ml_model), folds = 3L)
  spec$final <- "lm"
  env <- run_generated(ml_generate_code(spec, "mtcars"), "mtcars", mtcars)
  expect_setequal(env$comparacion$modelo, engines)
  expect_true(all(c("rmse", "mae", "r2") %in% names(env$comparacion)))
  expect_true(file.exists(file.path(tempdir(), "modelo_lm.rds")))
  base_rmse <- env$comparacion$rmse[env$comparacion$modelo == "baseline"]
  lm_rmse   <- env$comparacion$rmse[env$comparacion$modelo == "lm"]
  expect_lt(lm_rmse, base_rmse)
})

test_that("binaria: el script corre con predictores de texto", {
  skip_if_not_installed("pROC")
  engines <- engines_installed("binary")
  d <- datos_binarios()
  spec <- ml_spec("ocupado", c("edad", "sexo", "educ"), task = "binary",
                  positive = "Si", models = lapply(engines, ml_model))
  spec$final <- engines[length(engines)]
  env <- run_generated(ml_generate_code(spec, "encuesta"), "encuesta", d)
  expect_setequal(env$comparacion$modelo, engines)
  expect_true(all(env$comparacion$auc >= 0 & env$comparacion$auc <= 1))
  expect_equal(env$comparacion$auc[env$comparacion$modelo == "baseline"], 0.5)
  expect_identical(levels(env$datos_ml$ocupado), c("No", "Si"))
})

test_that("multiclase: el script corre y las probabilidades suman 1", {
  engines <- engines_installed("multiclass")
  spec <- ml_spec("Species", setdiff(names(iris), "Species"), task = "multiclass",
                  models = lapply(engines, ml_model))
  spec$final <- engines[length(engines)]
  env <- run_generated(ml_generate_code(spec, "iris"), "iris", iris)
  expect_setequal(env$comparacion$modelo, engines)
  for (m in engines) {
    a <- env$modelos[[m]]$ajustar(env$entrenamiento)
    p <- env$modelos[[m]]$predecir(a, env$test)
    expect_identical(colnames(p), levels(iris$Species))
    expect_equal(unname(rowSums(p)), rep(1, nrow(env$test)), tolerance = 1e-6)
  }
})

test_that("la imputacion simple conserva todas las filas", {
  d <- mtcars
  d$wt[c(2, 5, 9)] <- NA
  d$hp[c(3, 7)] <- NA
  spec <- ml_spec("mpg", c("wt", "hp"), task = "regression",
                  models = list(ml_model("baseline"), ml_model("lm")),
                  na_action = "impute_simple", folds = 3L)
  spec$final <- "lm"
  env <- run_generated(ml_generate_code(spec, "d"), "d", d)
  expect_identical(nrow(env$datos_ml), nrow(d))
  expect_false(anyNA(env$test_final[, c("wt", "hp")]))
})

test_that("la omision de NA elimina las filas incompletas", {
  d <- mtcars
  d$wt[c(2, 5)] <- NA
  spec <- ml_spec("mpg", c("wt", "hp"), task = "regression",
                  models = list(ml_model("baseline"), ml_model("lm")), folds = 3L)
  env <- run_generated(ml_generate_code(spec, "d"), "d", d)
  expect_identical(nrow(env$datos_ml), nrow(d) - 2L)
})

test_that("dos ejecuciones con la misma semilla dan las mismas metricas", {
  engines <- engines_installed("regression")
  spec <- ml_spec("mpg", c("wt", "hp", "cyl"), task = "regression",
                  models = lapply(engines, ml_model), folds = 3L)
  code <- ml_generate_code(spec, "mtcars")
  a <- run_generated(code, "mtcars", mtcars)$comparacion
  b <- run_generated(code, "mtcars", mtcars)$comparacion
  expect_identical(a, b)
})

test_that("xgboost: el script corre en las tres tareas", {
  skip_if_not_installed("xgboost")
  xgb <- function(...) ml_model("xgboost", list(nrounds = 20L, ...))

  # Regresion
  spec <- ml_spec("mpg", c("wt", "hp", "cyl"), task = "regression",
                  models = list(ml_model("baseline"), xgb()), folds = 3L)
  spec$final <- "xgboost"
  r <- ml_run(spec, mtcars)
  expect_true(r$ok, info = r$error)
  expect_true(is.numeric(r$final$predicted))
  expect_false(is.null(r$final$importance))

  # Multiclase: probabilidades n x K que suman 1 (no requiere pROC)
  spec <- ml_spec("Species", setdiff(names(iris), "Species"), task = "multiclass",
                  models = list(xgb()), folds = 3L)
  spec$final <- "xgboost"
  r <- ml_run(spec, iris)
  expect_true(r$ok, info = r$error)
  expect_identical(colnames(r$final$predicted), levels(iris$Species))
  expect_equal(unname(rowSums(r$final$predicted)), rep(1, r$n_test), tolerance = 1e-5)

  # Binaria (con predictores de texto -> matriz de disenio); AUC requiere pROC
  skip_if_not_installed("pROC")
  set.seed(11)
  d <- data.frame(edad = round(stats::runif(300, 18, 70)),
                  sexo = sample(c("H", "M"), 300, TRUE))
  d$y <- ifelse(stats::runif(300) < stats::plogis(-2 + 0.05 * d$edad), "Si", "No")
  spec <- ml_spec("y", c("edad", "sexo"), task = "binary", positive = "Si",
                  models = list(ml_model("baseline"), xgb()), folds = 3L)
  spec$final <- "xgboost"
  r <- ml_run(spec, d)
  expect_true(r$ok, info = r$error)
  expect_true(all(r$final$predicted >= 0 & r$final$predicted <= 1))
  expect_setequal(r$final$importance$variable, c("edad", "sexo"))
})
