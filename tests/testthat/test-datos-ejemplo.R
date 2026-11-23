# Tests con el juego de datos de ejemplo (inst/extdata): lo usan todos los
# modulos de RVisual, no solo el de Modelado.

ruta_ej <- function(archivo) {
  ruta <- system.file("extdata", archivo, package = "rvisual")
  if (!nzchar(ruta)) skip(paste("No se encuentra", archivo))
  ruta
}

ech <- function() readRDS(ruta_ej("ech_sintetica.rds"))

# -- Dataset: el mismo contenido en todos los formatos ------------------------

test_that("el dataset de ejemplo tiene la estructura documentada", {
  d <- ech()
  expect_identical(ncol(d), 17L)
  expect_gt(nrow(d), 2000)
  expect_identical(d$id_persona, seq_len(nrow(d)))
  expect_s3_class(d$fecha_entrevista, "Date")
  expect_setequal(unique(d$condicion_actividad), c("Ocupado", "Desocupado", "Inactivo"))
  expect_true(all(d$ingreso_laboral[d$ocupado == "No"] == 0))
  expect_true(anyNA(d$ingreso_laboral))
})

test_that("CSV (como lo lee el panel Dataset) coincide con el RDS", {
  d <- ech()
  csv <- utils::read.csv(ruta_ej("ech_sintetica.csv"), sep = ",", header = TRUE,
                         fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  expect_identical(dim(csv), dim(d))
  expect_identical(names(csv), names(d))
  expect_identical(csv$nivel_educ[!is.na(csv$nivel_educ) & csv$nivel_educ != ""],
                   d$nivel_educ[!is.na(d$nivel_educ)])
  expect_equal(csv$ingreso_laboral, d$ingreso_laboral)
})

test_that("Excel coincide con el RDS e incluye la tabla de departamentos", {
  skip_if_not_installed("readxl")
  d <- ech()
  x <- readxl::read_excel(ruta_ej("ech_sintetica.xlsx"))
  expect_identical(dim(x), dim(d))
  expect_equal(x$ingreso_laboral, d$ingreso_laboral)
  deptos <- readxl::read_excel(ruta_ej("ech_sintetica.xlsx"), sheet = "departamentos")
  expect_identical(nrow(deptos), 19L)
})

test_that("SPSS (.sav) coincide con el RDS", {
  skip_if_not_installed("haven")
  d <- ech()
  s <- haven::read_sav(ruta_ej("ech_sintetica.sav"))
  expect_identical(dim(s), dim(d))
  expect_equal(as.numeric(s$edad), as.numeric(d$edad))
})

test_that("SQLite tiene las tablas personas y departamentos", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ruta_ej("ech_sintetica.sqlite"))
  on.exit(DBI::dbDisconnect(con))
  expect_setequal(DBI::dbListTables(con), c("personas", "departamentos"))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM personas")$n
  expect_identical(as.integer(n), nrow(ech()))
})

# -- Constructor ----------------------------------------------------------------

test_that("Constructor: filtrar, recodificar y resumir sobre el ejemplo", {
  d <- ech()
  ops <- list(
    op_filter("ocupado", "==", "Si"),
    op_recode("sexo", list(Hombre = "H", Mujer = "M")),
    op_group_summarise("sexo", list(ingreso_medio = list(fn = "mean", col = "ingreso_laboral",
                                                          na_rm = TRUE)))
  )
  r <- apply_operations(d, ops)
  expect_identical(sort(r$sexo), c("H", "M"))
  expect_true(all(r$ingreso_medio > 0))
  expect_silent(parse(text = generate_code("ech", ops)))
})

test_that("Constructor: unir con la tabla de departamentos", {
  deptos <- utils::read.csv(ruta_ej("departamentos.csv"), fileEncoding = "UTF-8",
                            stringsAsFactors = FALSE)
  assign("departamentos_ej", deptos, envir = globalenv())
  on.exit(rm("departamentos_ej", envir = globalenv()))
  r <- apply_operations(ech(), list(op_join("departamentos_ej", "departamento", "left")))
  expect_false(anyNA(r$region))
  expect_setequal(unique(r$region), c("Montevideo", "Interior"))
})

# -- Modelado -------------------------------------------------------------------

ids_of <- function(d) vapply(d$findings, function(f) f$id, character(1))

test_that("diagnostico binario: identificador, alta cardinalidad y fuga", {
  d <- ml_diagnose(ech(), "ocupado",
                   c("id_persona", "localidad", "edad", "sexo", "nivel_educ",
                     "condicion_actividad"))
  expect_identical(d$task, "binary")
  expect_true(all(c("identifier", "high_cardinality", "leakage") %in% ids_of(d)))
  expect_setequal(d$suggested$exclude, c("id_persona", "localidad"))
})

test_that("diagnostico de regresion sobre el ingreso: muchos ceros y NA", {
  d <- ml_diagnose(ech(), "ingreso_laboral", c("edad", "sexo", "anios_educ"))
  expect_identical(d$task, "regression")
  expect_true(all(c("many_zeros", "target_na") %in% ids_of(d)))
})

test_that("diagnostico multiclase: clase minoritaria desbalanceada", {
  d <- ml_diagnose(ech(), "condicion_actividad", c("edad", "sexo", "anios_educ"))
  expect_identical(d$task, "multiclass")
  expect_true("imbalanced" %in% ids_of(d))
})

test_that("modelado de punta a punta sobre el ejemplo (binaria)", {
  skip_if_not_installed("pROC")
  dat <- ech()
  s <- ml_spec("ocupado", c("edad", "sexo", "anios_educ", "departamento"),
               task = "binary", positive = "Si",
               models = list(ml_model("baseline"), ml_model("glm"), ml_model("rpart")),
               folds = 3L)
  s$final <- "glm"
  r <- ml_run(s, dat, df_name = "ech")
  expect_true(r$ok)
  auc <- stats::setNames(r$comparison$auc, r$comparison$modelo)
  expect_gt(auc[["glm"]], auc[["baseline"]] + 0.1)
  expect_setequal(r$final$importance$variable,
                  c("edad", "sexo", "anios_educ", "departamento"))
})

test_that("modelado de regresion sobre ocupados, con un filtro del Constructor", {
  s <- ml_spec("ingreso_laboral", c("anios_educ", "horas_trabajadas", "sexo", "edad"),
               task = "regression",
               models = list(ml_model("baseline"), ml_model("lm")), folds = 3L)
  r <- ml_run(s, ech(), df_name = "ech", ops = list(op_filter("ocupado", "==", "Si")))
  expect_true(r$ok)
  r2 <- stats::setNames(r$comparison$r2, r$comparison$modelo)
  expect_gt(r2[["lm"]], 0.2)
})
