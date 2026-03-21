test_that("op_select crea estructura correcta", {
  op <- op_select(c("edad", "ingreso", "region"))
  expect_equal(op$type, "select")
  expect_equal(op$params$cols, c("edad", "ingreso", "region"))
  expect_true(grepl("edad", op$label))
})

test_that("op_filter crea estructura correcta", {
  op <- op_filter("anio", "==", "2025")
  expect_equal(op$type, "filter")
  expect_equal(op$params$col, "anio")
  expect_equal(op$params$op, "==")
})

test_that("op_filter rechaza operadores inválidos", {
  expect_error(op_filter("col", "LIKE", "valor"))
})

test_that("generate_code produce pipe correcto para select", {
  ops  <- list(op_select(c("a", "b")))
  code <- generate_code("mi_df", ops)
  expect_true(grepl("dplyr::select", code))
  expect_true(grepl("|>", code, fixed = TRUE))
  expect_true(grepl("mi_df", code))
})

test_that("generate_code con stack vacío devuelve comentario", {
  code <- generate_code("df_test", list())
  expect_true(grepl("^#", code))
})

test_that("apply_operations select filtra columnas correctamente", {
  df  <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  ops <- list(op_select(c("a", "c")))
  res <- apply_operations(df, ops)
  expect_equal(names(res), c("a", "c"))
  expect_equal(nrow(res), 3)
})

test_that("apply_operations filter filtra filas numéricas", {
  df  <- data.frame(valor = c(10, 20, 30), grupo = c("A", "B", "A"))
  ops <- list(op_filter("valor", ">", "15"))
  res <- apply_operations(df, ops)
  expect_equal(nrow(res), 2)
  expect_true(all(res$valor > 15))
})

test_that("discover_datasets encuentra data.frames en globalenv", {
  test_df <- data.frame(x = 1:5)
  assign("test_df_rvisual_internal", test_df, envir = .GlobalEnv)
  on.exit(rm("test_df_rvisual_internal", envir = .GlobalEnv))

  found <- discover_datasets()
  names <- vapply(found, function(d) d$name, character(1))
  expect_true("test_df_rvisual_internal" %in% names)
})

test_that("get_metadata devuelve estructura correcta", {
  df   <- data.frame(x = 1:10, y = letters[1:10], stringsAsFactors = FALSE)
  meta <- get_metadata(df, "test")
  expect_equal(meta$nrow, 10)
  expect_equal(meta$ncol, 2)
  expect_equal(nrow(meta$columns), 2)
})

test_that("extract_r_code_block extrae bloques de código", {
  text <- "Aquí el código:\n```r\nlibrary(dplyr)\ndf |> filter(x > 5)\n```"
  code <- rvisual:::extract_r_code_block(text)
  expect_true(grepl("filter", code))
})

test_that("load_file falla con extensión no soportada", {
  expect_error(load_file("/tmp/test.xyz", "xyz"), "Formato no soportado")
})
