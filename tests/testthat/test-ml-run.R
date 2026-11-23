# Tests de la ejecucion del modelado dentro del addin (incremento 4)

spec_mtcars <- function(final = NULL, ...) {
  s <- ml_spec("mpg", c("wt", "hp", "cyl"), task = "regression",
               models = list(ml_model("baseline"), ml_model("lm"), ml_model("rpart")),
               folds = 3L, ...)
  s$final <- final
  s
}

test_that("ml_run ejecuta y devuelve la comparacion", {
  r <- ml_run(spec_mtcars(), mtcars)
  expect_true(r$ok)
  expect_null(r$error)
  expect_setequal(r$comparison$modelo, c("baseline", "lm", "rpart"))
  expect_identical(nrow(r$cv_results), 9L)
  expect_identical(r$n_train + r$n_test, nrow(mtcars))
  expect_null(r$final)
  expect_true(is.numeric(r$elapsed))
})

test_that("coherencia: pantalla = script entregado ejecutado por separado", {
  spec <- spec_mtcars(final = "lm")
  r <- ml_run(spec, mtcars, df_name = "autos")

  ruta <- tempfile(fileext = ".rds")
  saveRDS(mtcars, ruta)
  script <- ml_generate_code(spec, "autos",
                             source = list(kind = "archivo", ext = "rds", filename = ruta))
  env <- new.env(parent = globalenv())
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  utils::capture.output(eval(parse(text = script), envir = env))

  expect_identical(r$comparison, env$comparacion)
  expect_identical(r$cv_results, env$resultados)
  expect_equal(r$final$metrics, env$metricas(env$test_final$mpg, env$pred_test))
})

test_that("el modelo final trae metricas, importancia y predicciones", {
  r <- ml_run(spec_mtcars(final = "rpart"), mtcars)
  expect_identical(r$final$engine, "rpart")
  expect_s3_class(r$final$fit, "rpart")
  expect_true(all(c("rmse", "mae", "r2") %in% names(r$final$metrics)))
  expect_null(r$final$confusion)
  expect_identical(length(r$final$observed), length(r$final$predicted))
  expect_identical(max(r$final$importance$importance), 100)
  expect_false(is.unsorted(rev(r$final$importance$importance)))
})

test_that("binaria: matriz de confusion e importancia por variable original", {
  skip_if_not_installed("pROC")
  set.seed(5)
  d <- data.frame(edad = round(stats::runif(300, 18, 70)),
                  sexo = sample(c("H", "M"), 300, TRUE),
                  educ = sample(c("Primaria", "Media", "Terciaria"), 300, TRUE))
  d$ocupado <- ifelse(stats::runif(300) < stats::plogis(-2 + 0.05 * d$edad), "Si", "No")
  s <- ml_spec("ocupado", c("edad", "sexo", "educ"), task = "binary", positive = "Si",
               models = list(ml_model("baseline"), ml_model("glm")))
  s$final <- "glm"
  r <- ml_run(s, d)
  expect_true(r$ok)
  expect_identical(dimnames(r$final$confusion)$predicho, c("No", "Si"))
  expect_identical(sum(r$final$confusion), r$n_test)
  expect_setequal(r$final$importance$variable, c("edad", "sexo", "educ"))
})

test_that("multiclase: matriz de confusion con todas las clases", {
  s <- ml_spec("Species", setdiff(names(iris), "Species"), task = "multiclass",
               models = list(ml_model("baseline"), ml_model("rpart")))
  s$final <- "rpart"
  r <- ml_run(s, iris)
  expect_true(r$ok)
  expect_identical(dim(r$final$confusion), c(3L, 3L))
  expect_identical(sum(r$final$confusion), r$n_test)
})

test_that("las operaciones del Constructor se aplican antes de modelar", {
  r <- ml_run(spec_mtcars(), mtcars, ops = list(op_filter("cyl", ">", 4)))
  expect_true(r$ok)
  expect_identical(r$n_train + r$n_test, sum(mtcars$cyl > 4))
})

test_that("una especificacion invalida no se ejecuta", {
  s <- spec_mtcars()
  s$predictors <- c("wt", "no_existe")
  r <- ml_run(s, mtcars)
  expect_false(r$ok)
  expect_match(r$error, "inexistentes")
  expect_null(r$code)
})

test_that("un error en tiempo de ejecucion se informa sin propagarse", {
  s <- spec_mtcars()
  s$predictors <- c("wt", "no_existe")
  r <- ml_run(s, mtcars, validate = FALSE)
  expect_false(r$ok)
  expect_match(r$error, "^Error al ejecutar el modelado")
})

test_that("los avisos de R se capturan", {
  d <- data.frame(x = c(1:20), y = c(rep("No", 10), rep("Si", 10)))
  s <- ml_spec("y", "x", task = "binary", positive = "Si",
               models = list(ml_model("baseline"), ml_model("glm")), folds = 2L)
  skip_if_not_installed("pROC")
  r <- ml_run(s, d)
  expect_true(r$ok)
  expect_true(length(r$warnings) > 0)
})

test_that("no modifica el entorno global, la semilla ni el disco", {
  set.seed(99)
  seed_before <- get(".Random.seed", envir = globalenv())
  globals_before <- ls(globalenv(), all.names = TRUE)
  files_before <- list.files(getwd())
  r <- ml_run(spec_mtcars(final = "lm"), mtcars)
  expect_true(r$ok)
  expect_identical(get(".Random.seed", envir = globalenv()), seed_before)
  expect_identical(ls(globalenv(), all.names = TRUE), globals_before)
  expect_identical(list.files(getwd()), files_before)
})

test_that("el codigo ejecutado no guarda el modelo", {
  r <- ml_run(spec_mtcars(final = "lm"), mtcars)
  expect_false(grepl("saveRDS", r$code))
  expect_match(ml_generate_code(spec_mtcars(final = "lm"), "mtcars"), "saveRDS", fixed = TRUE)
})

test_that("ml_run_notices avisa datasets grandes y muchos ajustes", {
  expect_length(ml_run_notices(mtcars, spec_mtcars()), 0)
  big <- data.frame(y = 1:10)
  expect_length(ml_run_notices(big, spec_mtcars(), max_rows = 5L), 1)
  s <- spec_mtcars()
  s$cv$folds <- 20L
  s$models <- rep(s$models, 1)
  s$models <- c(s$models, list(ml_model("ranger")))
  expect_true(any(grepl("modelos", ml_run_notices(mtcars, s))))
})

test_that("ml_importance devuelve NULL para la linea base", {
  expect_null(ml_importance("baseline", 3.5, c("x")))
})
