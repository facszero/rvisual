# tests/testthat/test-core.R
# Tests unitarios de RVisual — motor de operaciones y generación de código

# ── Dataset de prueba reutilizable ────────────────────────────────────────
make_df <- function() {
  data.frame(
    nombre   = c("Ana", "Bruno", "Carlos", "Diana", "Eva"),
    edad     = c(25L, 32L, 28L, 41L, 35L),
    ingreso  = c(30000, 55000, 42000, 80000, 61000),
    region   = c("Norte", "Sur", "Norte", "Centro", "Sur"),
    activo   = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

# ══════════════════════════════════════════════════════════════════════════
# op_select
# ══════════════════════════════════════════════════════════════════════════
test_that("op_select crea estructura correcta", {
  op <- op_select(c("edad", "ingreso", "region"))
  expect_equal(op$type, "select")
  expect_equal(op$params$cols, c("edad", "ingreso", "region"))
  expect_true(grepl("edad", op$label))
  expect_true(!is.null(op$created_at))
})

test_that("op_select falla con vector vacío", {
  expect_error(op_select(character(0)))
})

test_that("op_select falla con tipo incorrecto", {
  expect_error(op_select(123))
})

# ══════════════════════════════════════════════════════════════════════════
# op_filter
# ══════════════════════════════════════════════════════════════════════════
test_that("op_filter crea estructura correcta", {
  op <- op_filter("edad", ">=", "30")
  expect_equal(op$type, "filter")
  expect_equal(op$params$col, "edad")
  expect_equal(op$params$op, ">=")
  expect_equal(op$params$value, "30")
})

test_that("op_filter acepta todos los operadores válidos", {
  for (oper in c("==", "!=", ">", "<", ">=", "<=")) {
    expect_no_error(op_filter("col", oper, "1"))
  }
})

test_that("op_filter rechaza operadores inválidos", {
  expect_error(op_filter("col", "LIKE", "valor"))
  expect_error(op_filter("col", "IN",   "valor"))
})

# ══════════════════════════════════════════════════════════════════════════
# op_arrange
# ══════════════════════════════════════════════════════════════════════════
test_that("op_arrange ascendente", {
  op <- op_arrange("ingreso")
  expect_equal(op$type, "arrange")
  expect_false(op$params$desc)
  expect_true(grepl("ingreso", op$label))
})

test_that("op_arrange descendente menciona desc en label", {
  op <- op_arrange("ingreso", desc = TRUE)
  expect_true(op$params$desc)
  expect_true(grepl("desc", op$label))
})

# ══════════════════════════════════════════════════════════════════════════
# op_mutate
# ══════════════════════════════════════════════════════════════════════════
test_that("op_mutate crea estructura correcta", {
  op <- op_mutate("ingreso_anual", "ingreso * 12")
  expect_equal(op$type, "mutate")
  expect_equal(op$params$new_col, "ingreso_anual")
  expect_equal(op$params$expression, "ingreso * 12")
})

# ══════════════════════════════════════════════════════════════════════════
# op_rename
# ══════════════════════════════════════════════════════════════════════════
test_that("op_rename crea estructura correcta", {
  op <- op_rename("nombre", "nombre_completo")
  expect_equal(op$type, "rename")
  expect_equal(op$params$old_name, "nombre")
  expect_equal(op$params$new_name, "nombre_completo")
})

# ══════════════════════════════════════════════════════════════════════════
# op_recode
# ══════════════════════════════════════════════════════════════════════════
test_that("op_recode crea estructura correcta", {
  mapping <- list("Norte" = "N", "Sur" = "S")
  op <- op_recode("region", mapping)
  expect_equal(op$type, "recode")
  expect_equal(op$params$col, "region")
  expect_equal(length(op$params$mapping), 2)
})

# ══════════════════════════════════════════════════════════════════════════
# op_group_summarise
# ══════════════════════════════════════════════════════════════════════════
test_that("op_group_summarise crea estructura correcta", {
  fns <- list(
    ing_medio = list(fn = "mean", col = "ingreso", na_rm = TRUE),
    cantidad  = list(fn = "n",    col = NULL,      na_rm = FALSE)
  )
  op <- op_group_summarise(c("region"), fns)
  expect_equal(op$type, "group_summarise")
  expect_equal(op$params$group_cols, "region")
  expect_equal(length(op$params$summary_fns), 2)
})

# ══════════════════════════════════════════════════════════════════════════
# apply_operations
# ══════════════════════════════════════════════════════════════════════════
test_that("apply_operations con stack vacío devuelve df original", {
  df  <- make_df()
  res <- apply_operations(df, list())
  expect_equal(res, df)
})

test_that("apply_operations select filtra columnas", {
  df  <- make_df()
  res <- apply_operations(df, list(op_select(c("nombre", "edad"))))
  expect_equal(names(res), c("nombre", "edad"))
  expect_equal(nrow(res), 5)
})

test_that("apply_operations filter numérico", {
  df  <- make_df()
  res <- apply_operations(df, list(op_filter("ingreso", ">", "50000")))
  expect_equal(nrow(res), 3)
  expect_true(all(res$ingreso > 50000))
})

test_that("apply_operations filter textual exacto", {
  df  <- make_df()
  res <- apply_operations(df, list(op_filter("region", "==", "Sur")))
  expect_equal(nrow(res), 2)
  expect_true(all(res$region == "Sur"))
})

test_that("apply_operations arrange ascendente", {
  df  <- make_df()
  res <- apply_operations(df, list(op_arrange("ingreso")))
  expect_equal(res$ingreso, sort(df$ingreso))
})

test_that("apply_operations arrange descendente", {
  df  <- make_df()
  res <- apply_operations(df, list(op_arrange("ingreso", desc = TRUE)))
  expect_equal(res$ingreso, sort(df$ingreso, decreasing = TRUE))
})

test_that("apply_operations mutate crea nueva columna", {
  df  <- make_df()
  res <- apply_operations(df, list(op_mutate("ing_k", "ingreso / 1000")))
  expect_true("ing_k" %in% names(res))
  expect_equal(res$ing_k, df$ingreso / 1000)
})

test_that("apply_operations stack múltiple funciona en cadena", {
  df  <- make_df()
  ops <- list(
    op_filter("activo", "==", "TRUE"),
    op_select(c("nombre", "ingreso", "region")),
    op_arrange("ingreso", desc = TRUE)
  )
  res <- apply_operations(df, ops)
  expect_equal(names(res), c("nombre", "ingreso", "region"))
  expect_true(nrow(res) < nrow(df))
  expect_equal(res$ingreso, sort(res$ingreso, decreasing = TRUE))
})

test_that("apply_operations group_summarise agrupa y cuenta", {
  df  <- make_df()
  fns <- list(n_personas = list(fn = "n", col = NULL, na_rm = FALSE))
  ops <- list(op_group_summarise("region", fns))
  res <- apply_operations(df, ops)
  expect_equal(sort(names(res)), sort(c("region", "n_personas")))
  expect_equal(sum(res$n_personas), 5L)
})

test_that("apply_operations group_summarise calcula media", {
  df  <- make_df()
  fns <- list(ing_medio = list(fn = "mean", col = "ingreso", na_rm = TRUE))
  ops <- list(op_group_summarise("region", fns))
  res <- apply_operations(df, ops)
  norte_esperado <- mean(df$ingreso[df$region == "Norte"])
  norte_real     <- res$ing_medio[res$region == "Norte"]
  expect_equal(norte_real, norte_esperado)
})

# ══════════════════════════════════════════════════════════════════════════
# generate_code
# ══════════════════════════════════════════════════════════════════════════
test_that("generate_code stack vacío devuelve comentario", {
  code <- generate_code("mi_df", list())
  expect_true(startsWith(trimws(code), "#"))
})

test_that("generate_code select produce dplyr::select con pipe", {
  code <- generate_code("mi_df", list(op_select(c("a", "b"))))
  expect_true(grepl("dplyr::select", code))
  expect_true(grepl("|>", code, fixed = TRUE))
  expect_true(grepl("mi_df", code))
})

test_that("generate_code filter produce dplyr::filter", {
  code <- generate_code("df", list(op_filter("x", ">", "10")))
  expect_true(grepl("dplyr::filter", code))
  expect_true(grepl("x > 10", code))
})

test_that("generate_code arrange ascendente no tiene desc()", {
  code <- generate_code("df", list(op_arrange("col")))
  expect_true(grepl("dplyr::arrange", code))
  expect_false(grepl("desc", code))
})

test_that("generate_code arrange descendente tiene desc()", {
  code <- generate_code("df", list(op_arrange("col", desc = TRUE)))
  expect_true(grepl("dplyr::desc", code))
})

test_that("generate_code mutate incluye nombre y expresión", {
  code <- generate_code("df", list(op_mutate("total", "a + b")))
  expect_true(grepl("dplyr::mutate", code))
  expect_true(grepl("total", code))
  expect_true(grepl("a \\+ b", code))
})

test_that("generate_code group_summarise incluye group_by y summarise", {
  fns  <- list(media = list(fn = "mean", col = "ingreso", na_rm = TRUE))
  code <- generate_code("df", list(op_group_summarise("region", fns)))
  expect_true(grepl("dplyr::group_by", code))
  expect_true(grepl("dplyr::summarise", code))
  expect_true(grepl("mean\\(ingreso", code))
})

test_that("generate_code conteo usa dplyr::n()", {
  fns  <- list(n = list(fn = "n", col = NULL, na_rm = FALSE))
  code <- generate_code("df", list(op_group_summarise("region", fns)))
  expect_true(grepl("dplyr::n()", code, fixed = TRUE))
})

test_that("generate_code con assign_to genera asignación", {
  code <- generate_code("df", list(op_select(c("a"))), assign_to = "resultado")
  expect_true(startsWith(trimws(gsub("^#.*\n", "", code)), "resultado <-") ||
              grepl("resultado <-", code))
})

test_that("generate_code stack múltiple encadena pipes", {
  ops  <- list(op_filter("x", ">", "5"), op_select(c("x", "y")))
  code <- generate_code("df", ops)
  # Debe haber al menos 2 pipes
  expect_gte(lengths(regmatches(code, gregexpr("|>", code, fixed = TRUE))), 2)
})

# ══════════════════════════════════════════════════════════════════════════
# get_metadata
# ══════════════════════════════════════════════════════════════════════════
test_that("get_metadata devuelve dimensiones correctas", {
  df   <- make_df()
  meta <- get_metadata(df, "prueba")
  expect_equal(meta$nrow, 5)
  expect_equal(meta$ncol, 5)
  expect_equal(meta$name, "prueba")
})

test_that("get_metadata columnas tiene las columnas esperadas", {
  df   <- make_df()
  meta <- get_metadata(df, "prueba")
  expect_equal(nrow(meta$columns), 5)
  expect_true(all(c("name", "type", "visual", "unique", "na_pct") %in%
                  names(meta$columns)))
})

test_that("get_metadata detecta NA correctamente", {
  df      <- data.frame(x = c(1, NA, 3), y = c("a", "b", "c"))
  meta    <- get_metadata(df, "test")
  na_fila <- meta$columns[meta$columns$name == "x", "na_pct"]
  expect_equal(na_fila, round(1/3 * 100, 1))
})

test_that("get_metadata clasifica tipos visuales correctamente", {
  df   <- data.frame(
    num  = 1:5,
    txt  = letters[1:5],
    fct  = factor(letters[1:5]),
    lgl  = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  meta <- get_metadata(df, "tipos")
  tipos <- setNames(meta$columns$visual, meta$columns$name)
  expect_true(grepl("Num", tipos["num"]))
  expect_true(grepl("Texto|texto", tipos["txt"], ignore.case = TRUE))
  expect_true(grepl("factor|Categ", tipos["fct"], ignore.case = TRUE))
  expect_true(grepl("L.gica|logica", tipos["lgl"], ignore.case = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════
# discover_datasets
# ══════════════════════════════════════════════════════════════════════════
test_that("discover_datasets encuentra data.frames en .GlobalEnv", {
  assign("__rvisual_test_df__", make_df(), envir = .GlobalEnv)
  on.exit(rm("__rvisual_test_df__", envir = .GlobalEnv))

  found <- discover_datasets()
  nms   <- vapply(found, function(d) d$name, character(1))
  expect_true("__rvisual_test_df__" %in% nms)
})

test_that("discover_datasets ignora no data.frames", {
  assign("__rvisual_test_vec__", 1:10, envir = .GlobalEnv)
  on.exit(rm("__rvisual_test_vec__", envir = .GlobalEnv))

  found <- discover_datasets()
  nms   <- vapply(found, function(d) d$name, character(1))
  expect_false("__rvisual_test_vec__" %in% nms)
})

# ══════════════════════════════════════════════════════════════════════════
# extract_r_code_block (función interna)
# ══════════════════════════════════════════════════════════════════════════
test_that("extract_r_code_block extrae bloque ```r", {
  text <- "Explicación:\n```r\nlibrary(dplyr)\ndf |> filter(x > 5)\n```"
  code <- rvisual:::extract_r_code_block(text)
  expect_true(grepl("filter", code))
  expect_false(grepl("```", code))
})

test_that("extract_r_code_block extrae bloque sin lenguaje", {
  text <- "Código:\n```\nx <- 1 + 1\n```"
  code <- rvisual:::extract_r_code_block(text)
  expect_true(grepl("x <- 1", code))
})

test_that("extract_r_code_block retorna NULL si no hay bloque", {
  text <- "Solo texto sin código."
  code <- rvisual:::extract_r_code_block(text)
  expect_null(code)
})

test_that("extract_r_code_block maneja código multilínea", {
  text <- "```r\na <- 1\nb <- 2\nc <- a + b\n```"
  code <- rvisual:::extract_r_code_block(text)
  expect_true(grepl("a <- 1", code))
  expect_true(grepl("c <- a \\+ b", code))
})

# ══════════════════════════════════════════════════════════════════════════
# config (guardar y cargar)
# ══════════════════════════════════════════════════════════════════════════
test_that("config_save y config_load son inversos", {
  cfg_orig <- list(provider = "openai", model = "gpt-4o", api_key = "test-key")
  config_save(cfg_orig)
  cfg_leido <- config_load()
  expect_equal(cfg_leido$provider, "openai")
  expect_equal(cfg_leido$model,    "gpt-4o")
})

test_that("config_load retorna lista vacía si no existe config", {
  # Simular que no existe el archivo
  path <- rvisual:::config_path()
  if (file.exists(path)) {
    backup <- readLines(path)
    file.remove(path)
    on.exit({
      writeLines(backup, path)
    })
  }
  result <- config_load()
  expect_true(is.list(result))
})

# ══════════════════════════════════════════════════════════════════════════
# load_file
# ══════════════════════════════════════════════════════════════════════════
test_that("load_file falla con extensión no soportada", {
  expect_error(load_file("/tmp/test.xyz", "xyz"), "Formato no soportado")
})

test_that("load_file carga CSV correctamente", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(make_df(), tmp, row.names = FALSE)
  on.exit(unlink(tmp))
  # load_file usa readr pero podemos probar que no falla y devuelve df
  skip_if_not_installed("readr")
  df <- load_file(tmp, "csv")
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 5)
})

test_that("load_file carga RDS correctamente", {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(make_df(), tmp)
  on.exit(unlink(tmp))
  df <- load_file(tmp, "rds")
  expect_equal(nrow(df), 5)
  expect_equal(names(df), names(make_df()))
})
