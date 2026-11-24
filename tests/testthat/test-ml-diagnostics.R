# Tests del diagnostico del panel Modelado (incremento 3)

ids_of <- function(d) vapply(d$findings, function(f) f$id, character(1))

finding <- function(d, id) {
  hit <- Filter(function(f) f$id == id, d$findings)
  if (length(hit) == 0) NULL else hit[[1]]
}

# Dataset "sano": 400 filas, objetivo numerico sin ceros ni asimetria,
# predictores sin NA ni identificadores.
sano <- function(n = 400) {
  set.seed(7)
  d <- data.frame(
    x1  = stats::rnorm(n),
    x2  = stats::runif(n),
    grp = sample(c("a", "b", "c"), n, TRUE),
    stringsAsFactors = FALSE
  )
  d$y <- 10 + 2 * d$x1 + d$x2 + stats::rnorm(n)
  d
}

clasif <- function(n = 400, p_pos = 0.4) {
  set.seed(8)
  d <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n),
                  grp = sample(c("a", "b", "c"), n, TRUE), stringsAsFactors = FALSE)
  d$y <- ifelse(stats::runif(n) < p_pos, "Si", "No")
  d
}

test_that("un dataset sano solo produce el hallazgo informativo fijo", {
  d <- ml_diagnose(sano(), "y", c("x1", "x2", "grp"))
  expect_identical(d$task, "regression")
  expect_identical(ids_of(d), "nonlinear")
  expect_identical(d$suggested$exclude, character(0))
  expect_identical(d$suggested$folds, 5L)
  expect_true(all(c("baseline", "lm", "rpart") %in% d$suggested$engines))
})

test_that("el perfil describe columnas y objetivo", {
  dat <- sano()
  dat$x2[1:40] <- NA
  p <- ml_profile(dat, "y", c("x1", "x2", "grp"))
  expect_identical(p$n, 400L)
  expect_identical(p$columns$type, c("numerica", "numerica", "categorica"))
  expect_equal(p$columns$na_share[2], 0.1)
  expect_identical(p$columns$n_unique[3], 3L)
  expect_identical(p$target_summary$kind, "numeric")
  pc <- ml_profile(clasif(), "y", c("x1"))
  expect_identical(pc$target_summary$kind, "categorical")
  expect_equal(sum(pc$target_summary$shares), 1)
})

test_that("objetivo inutilizable produce un hallazgo critico y nada mas", {
  dat <- sano()
  dat$y <- 1
  d <- ml_diagnose(dat, "y", c("x1"))
  expect_true(is.na(d$task))
  expect_identical(ids_of(d), "target_unusable")
  expect_identical(d$findings[[1]]$severity, "critical")
})

test_that("pocas filas sugiere 10 folds", {
  d <- ml_diagnose(sano(100), "y", c("x1", "x2"))
  expect_true("few_rows" %in% ids_of(d))
  expect_identical(d$suggested$folds, 10L)
})

test_that("muchos predictores pre-marca glmnet", {
  dat <- sano(250)
  for (i in 1:30) dat[[paste0("z", i)]] <- stats::rnorm(250)
  d <- ml_diagnose(dat, "y", setdiff(names(dat), "y"))
  expect_true("many_predictors" %in% ids_of(d))
  skip_if_not_installed("glmnet")
  expect_true("glmnet" %in% d$suggested$engines)
})

test_that("clases desbalanceadas se detectan", {
  d <- ml_diagnose(clasif(p_pos = 0.05), "y", c("x1", "x2"))
  expect_identical(d$task, "binary")
  expect_true("imbalanced" %in% ids_of(d))
  expect_false("imbalanced" %in% ids_of(ml_diagnose(clasif(), "y", c("x1"))))
})

test_that("una clase con muy pocos casos es critica", {
  dat <- clasif()
  dat$y[1:12] <- "Quizas"
  d <- ml_diagnose(dat, "y", c("x1"))
  expect_identical(d$task, "multiclass")
  f <- finding(d, "small_class")
  expect_false(is.null(f))
  expect_identical(f$severity, "critical")
  expect_identical(d$findings[[1]]$severity, "critical")
})

test_that("muchos ceros en el objetivo se detectan", {
  dat <- sano()
  dat$y[1:200] <- 0
  expect_true("many_zeros" %in% ids_of(ml_diagnose(dat, "y", c("x1"))))
})

test_that("un objetivo positivo muy asimetrico sugiere log", {
  dat <- sano()
  set.seed(3)
  dat$y <- exp(stats::rnorm(400, sd = 1.5))
  expect_true("skewed_target" %in% ids_of(ml_diagnose(dat, "y", c("x1"))))
})

test_that("NA moderados informan y NA masivos excluyen", {
  dat <- sano()
  dat$x1[1:40] <- NA
  dat$x2[1:250] <- NA
  d <- ml_diagnose(dat, "y", c("x1", "x2", "grp"))
  expect_true(all(c("na_predictor", "na_massive", "rows_lost") %in% ids_of(d)))
  expect_identical(d$suggested$exclude, "x2")
})

test_that("las filas sin objetivo se informan", {
  dat <- sano()
  dat$y[1:10] <- NA
  expect_true("target_na" %in% ids_of(ml_diagnose(dat, "y", c("x1"))))
})

test_that("identificadores se excluyen por defecto", {
  dat <- sano()
  dat$id_hogar <- sample(seq_len(400))
  dat$codigo <- paste0("H", seq_len(400))
  d <- ml_diagnose(dat, "y", c("x1", "id_hogar", "codigo"))
  expect_setequal(d$suggested$exclude, c("id_hogar", "codigo"))
  expect_false(ml_looks_like_id(stats::rnorm(400)))
  expect_false(ml_looks_like_id(c(1:5)))
})

test_that("factores de alta cardinalidad se excluyen", {
  dat <- sano()
  dat$localidad <- sample(paste0("L", 1:80), 400, TRUE)
  d <- ml_diagnose(dat, "y", c("x1", "localidad"))
  expect_true("high_cardinality" %in% ids_of(d))
  expect_identical(d$suggested$exclude, "localidad")
})

test_that("posible fuga: numerica muy correlacionada con el objetivo", {
  dat <- sano()
  dat$y_copia <- dat$y * 1.01 + 0.001
  f <- finding(ml_diagnose(dat, "y", c("x1", "y_copia")), "leakage")
  expect_false(is.null(f))
  expect_match(f$message, "y_copia")
  expect_true("y_copia" %in% ml_diagnose(dat, "y", c("x1", "y_copia"))$suggested$exclude)
})

test_that("ml_single_auc mide la separacion de un predictor aislado", {
  expect_equal(ml_single_auc(c(1, 2, 3, 4), c(0, 0, 1, 1)), 1)
  expect_equal(ml_single_auc(c(4, 3, 2, 1), c(0, 0, 1, 1)), 0)
  expect_equal(ml_single_auc(c(1, 2, 1, 2), c(0, 0, 1, 1)), 0.5)
  expect_true(is.na(ml_single_auc(c(1, 1, 1), c(0, 1, 1))))
})

test_that("posible fuga: categoria que determina la clase", {
  dat <- clasif()
  dat$estado_final <- ifelse(dat$y == "Si", "cerrado_si", "cerrado_no")
  d <- ml_diagnose(dat, "y", c("x1", "estado_final"))
  expect_true("leakage" %in% ids_of(d))
  expect_false("leakage" %in% ids_of(ml_diagnose(dat, "y", c("x1", "grp"))))
})

test_that("los hallazgos se ordenan por severidad", {
  dat <- clasif()
  dat$y[1:12] <- "Quizas"
  dat$x1[1:40] <- NA
  sev <- vapply(ml_diagnose(dat, "y", c("x1", "x2"))$findings,
                function(f) f$severity, character(1))
  rank <- match(sev, c("critical", "warning", "info"))
  expect_false(is.unsorted(rank))
})

test_that("los umbrales son configurables", {
  th <- ml_diag_thresholds()
  th$few_rows <- 1000
  expect_true("few_rows" %in% ids_of(ml_diagnose(sano(), "y", c("x1"), thresholds = th)))
})

test_that("ml_spec_from_diagnosis produce una especificacion valida", {
  dat <- sano()
  dat$id_hogar <- sample(seq_len(400))
  d <- ml_diagnose(dat, "y", c("x1", "x2", "grp", "id_hogar"))
  spec <- ml_spec_from_diagnosis(d, "y", c("x1", "x2", "grp", "id_hogar"))
  expect_false("id_hogar" %in% spec$predictors)
  v <- ml_spec_validate(spec, data = dat)
  expect_true(v$valid)
})

test_that("el perfil admite una lista vacia de predictores", {
  p <- ml_profile(sano(), "y", character(0))
  expect_identical(nrow(p$columns), 0L)
})

test_that("la regla de clases chicas usa los folds sugeridos", {
  dat <- clasif(150)
  dat$y <- c(rep("Si", 30), rep("No", 120))
  d5 <- ml_diagnose(dat, "y", c("x1"), thresholds = modifyList(ml_diag_thresholds(), list(few_rows = 100)))
  expect_false("small_class" %in% ids_of(d5))
  d10 <- ml_diagnose(dat, "y", c("x1"))
  expect_identical(d10$suggested$folds, 10L)
  expect_true("small_class" %in% ids_of(d10))
  expect_match(finding(d10, "small_class")$evidence, "10 folds")
})
