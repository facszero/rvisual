# Tests del modulo de Modelado: catalogo de motores y ml_spec (incremento 1)

# -- Catalogo -----------------------------------------------------------

test_that("el catalogo tiene estructura completa y coherente", {
  catalog <- ml_engine_catalog()
  expect_true(all(c("baseline", "lm", "glm", "glmnet", "rpart", "ranger", "xgboost")
                  %in% names(catalog)))
  for (nm in names(catalog)) {
    e <- catalog[[nm]]
    expect_identical(e$id, nm)
    expect_true(is.character(e$label) && nzchar(e$label))
    expect_true(is.null(e$package) || is.character(e$package))
    expect_true(length(e$tasks) > 0 && all(e$tasks %in% ml_tasks()))
    for (p in e$params) {
      expect_true(p$type %in% c("integer", "numeric", "choice"))
    }
  }
})

test_that("ml_engine_get falla con motor desconocido", {
  expect_error(ml_engine_get("svm"), "Motor desconocido")
  expect_error(ml_engine_get(c("lm", "glm")), "Motor desconocido")
})

test_that("ml_engines_for_task respeta las tareas de cada motor", {
  reg <- ml_engines_for_task("regression")
  bin <- ml_engines_for_task("binary")
  mul <- ml_engines_for_task("multiclass")
  expect_true("lm" %in% reg && !"glm" %in% reg)
  expect_true("glm" %in% bin && !"lm" %in% bin)
  expect_false(any(c("lm", "glm") %in% mul))
  expect_identical(reg[1], "baseline")
  expect_error(ml_engines_for_task("clustering"))
})

test_that("los motores de base R siempre estan disponibles", {
  expect_true(ml_engine_available("baseline"))
  expect_true(ml_engine_available("lm"))
  expect_true(ml_engine_available("glm"))
})

test_that("ml_required_packages agrega pROC solo en binaria", {
  expect_identical(ml_required_packages(c("baseline", "lm"), "regression"), character(0))
  expect_identical(ml_required_packages(c("glm", "ranger"), "binary"), c("ranger", "pROC"))
  expect_identical(ml_required_packages(c("rpart", "rpart"), "multiclass"), "rpart")
})

test_that("ml_engine_defaults conserva los NULL", {
  d <- ml_engine_defaults("ranger")
  expect_true("mtry" %in% names(d))
  expect_null(d$mtry)
  expect_identical(d$num.trees, 500L)
  expect_length(ml_engine_defaults("lm"), 0)
})

test_that("ml_metrics_for_task devuelve la metrica principal primero", {
  expect_identical(ml_metrics_for_task("regression")[1], "rmse")
  expect_identical(ml_metrics_for_task("binary")[1], "auc")
  expect_identical(ml_metrics_for_task("multiclass")[1], "f1_macro")
})

# -- Deteccion de tarea --------------------------------------------------

test_that("ml_detect_task infiere el tipo de tarea", {
  expect_identical(ml_detect_task(mtcars$mpg), "regression")
  expect_identical(ml_detect_task(mtcars$am), "binary")
  expect_identical(ml_detect_task(c("Si", "No", NA, "Si")), "binary")
  expect_identical(ml_detect_task(c(TRUE, FALSE)), "binary")
  expect_identical(ml_detect_task(iris$Species), "multiclass")
  expect_identical(ml_detect_task(factor(c(1, 2, 3, 4))), "multiclass")
})

test_that("ml_detect_task devuelve NA para objetivos no utilizables", {
  expect_true(is.na(ml_detect_task(rep(1, 10))))
  expect_true(is.na(ml_detect_task(c(NA, NA))))
  expect_true(is.na(ml_detect_task(as.character(1:60))))
})

# -- ml_model ------------------------------------------------------------

test_that("ml_model completa con valores por defecto", {
  m <- ml_model("rpart", list(cp = 0.05))
  expect_identical(m$engine, "rpart")
  expect_identical(m$params$cp, 0.05)
  expect_identical(m$params$maxdepth, 10L)
})

test_that("ml_model rechaza hiperparametros desconocidos", {
  expect_error(ml_model("rpart", list(ntree = 10)), "desconocidos")
})

# -- ml_spec y validacion -------------------------------------------------

spec_reg <- function(...) {
  ml_spec("mpg", c("wt", "hp", "cyl"), task = "regression",
          models = list(ml_model("baseline"), ml_model("lm")), ...)
}

test_that("ml_spec construye la estructura esperada", {
  s <- spec_reg()
  expect_identical(s$version, ml_spec_version())
  expect_identical(s$task, "regression")
  expect_false(s$split$stratify)
  expect_identical(s$metrics, ml_metrics_for_task("regression"))
  expect_null(s$final)
})

test_that("ml_spec estratifica por defecto en clasificacion", {
  s <- ml_spec("am", c("wt", "hp"), task = "binary", positive = "1",
               models = list(ml_model("baseline"), ml_model("glm")))
  expect_true(s$split$stratify)
})

test_that("ml_spec por defecto solo incluye motores instalados", {
  s <- ml_spec("mpg", c("wt", "hp"), task = "regression")
  engines <- vapply(s$models, function(m) m$engine, character(1))
  expect_true(all(vapply(engines, ml_engine_available, logical(1))))
  expect_true(all(c("baseline", "lm") %in% engines))
})

test_that("una especificacion correcta valida sin errores", {
  v <- ml_spec_validate(spec_reg(), data = mtcars)
  expect_true(v$valid)
  expect_length(v$errors, 0)
})

test_that("la validacion detecta problemas de estructura", {
  s <- spec_reg()
  s$predictors <- c("wt", "mpg")
  s$split$test_prop <- 0.8
  s$cv$folds <- 1L
  s$na_action <- "drop"
  v <- ml_spec_validate(s)
  expect_false(v$valid)
  expect_true(any(grepl("objetivo no puede", v$errors)))
  expect_true(any(grepl("test", v$errors)))
  expect_true(any(grepl("folds", v$errors)))
  expect_true(any(grepl("NA", v$errors)))
})

test_that("la validacion rechaza motores incompatibles con la tarea", {
  s <- spec_reg()
  s$models <- list(ml_model("baseline"), ml_model("glm"))
  v <- ml_spec_validate(s)
  expect_false(v$valid)
  expect_true(any(grepl("no admite", v$errors)))
})

test_that("la validacion detecta hiperparametros fuera de rango", {
  s <- spec_reg()
  s$models <- list(ml_model("rpart", list(maxdepth = 0L)))
  v <- ml_spec_validate(s, check_packages = FALSE)
  expect_true(any(grepl("fuera de rango", v$errors)))
})

test_that("la validacion advierte si falta la linea base", {
  s <- spec_reg()
  s$models <- list(ml_model("lm"))
  v <- ml_spec_validate(s)
  expect_true(v$valid)
  expect_true(any(grepl("l\u00ednea base", v$warnings)))
})

test_that("binaria exige clase positiva existente en los datos", {
  d <- data.frame(y = c("Si", "No", "Si", "No"), x = 1:4)
  d <- d[rep(1:4, 5), ]
  s <- ml_spec("y", "x", task = "binary", positive = NULL,
               models = list(ml_model("baseline"), ml_model("glm")))
  expect_true(any(grepl("clase positiva", ml_spec_validate(s)$errors)))
  s$positive <- "Quizas"
  expect_true(any(grepl("no aparece", ml_spec_validate(s, data = d)$errors)))
  s$positive <- "Si"
  expect_true(ml_spec_validate(s, data = d)$valid)
})

test_that("la validacion contra datos detecta columnas y tipos incoherentes", {
  s <- spec_reg()
  s$predictors <- c("wt", "no_existe")
  expect_true(any(grepl("inexistentes", ml_spec_validate(s, data = mtcars)$errors)))

  s <- ml_spec("Species", c("Sepal.Length"), task = "regression",
               models = list(ml_model("baseline")))
  expect_true(any(grepl("num\u00e9rica", ml_spec_validate(s, data = iris)$errors)))

  s <- ml_spec("am", c("wt"), task = "multiclass",
               models = list(ml_model("baseline")))
  expect_true(any(grepl("binaria", ml_spec_validate(s, data = mtcars)$errors)))
})

test_that("la validacion informa filas con NA que se eliminaran", {
  d <- mtcars
  d$wt[1:4] <- NA
  v <- ml_spec_validate(spec_reg(), data = d)
  expect_true(v$valid)
  expect_true(any(grepl("4 filas con NA", v$warnings)))
})

test_that("la validacion exige filas suficientes para los folds", {
  v <- ml_spec_validate(spec_reg(), data = mtcars[1:6, ])
  expect_false(v$valid)
  expect_true(any(grepl("pocas filas", v$errors)))
})

test_that("la validacion detecta paquetes faltantes", {
  skip_if(requireNamespace("xgboost", quietly = TRUE), "xgboost instalado")
  s <- spec_reg()
  s$models <- list(ml_model("baseline"), ml_model("xgboost"))
  v <- ml_spec_validate(s)
  expect_true(any(grepl("Falta instalar", v$errors)))
})
