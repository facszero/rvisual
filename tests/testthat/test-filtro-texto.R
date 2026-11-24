# Filtros y recodificaciones sobre columnas de texto con valores que
# parecen numeros ("01", "0611100701"): no deben convertirse a numero.

df_codigos <- function() {
  data.frame(division = c("01", "02", "01", "10"),
             variedad = c("0111100101", "0211100101", "0111100201", "1011100101"),
             n = c(1, 2, 3, 4), activo = c(TRUE, FALSE, TRUE, TRUE),
             stringsAsFactors = FALSE)
}

test_that("filtrar texto con ceros a la izquierda conserva las filas", {
  d <- df_codigos()
  res <- apply_operations(d, list(op_filter("division", "==", "01", numeric = FALSE)))
  expect_equal(res$n, c(1, 3))
  # aun sin el argumento numeric, la vista previa usa el tipo de la columna
  res2 <- apply_operations(d, list(op_filter("variedad", "!=", "0111100101")))
  expect_equal(res2$n, c(2, 3, 4))
})

test_that("el codigo del filtro pone el valor entre comillas si la columna es texto", {
  expect_equal(code_filter(op_filter("division", "==", "01", numeric = FALSE)$params),
               'dplyr::filter(division == "01")')
  expect_equal(code_filter(op_filter("n", ">", "2", numeric = TRUE)$params),
               "dplyr::filter(n > 2)")
  # sin el argumento: mismo codigo que antes
  expect_equal(code_filter(op_filter("n", ">", "2")$params), "dplyr::filter(n > 2)")
  expect_equal(code_filter(op_filter("r", "==", "Sur")$params),
               'dplyr::filter(r == "Sur")')
})

test_that("el codigo del filtro da el mismo resultado que la vista previa", {
  d <- df_codigos()
  for (op in list(op_filter("division", "==", "01", numeric = FALSE),
                  op_filter("n", ">=", "2", numeric = TRUE),
                  op_filter("activo", "==", "TRUE", numeric = FALSE))) {
    via_codigo <- eval(parse(text = paste("d |>", code_filter(op$params))))
    expect_equal(via_codigo, apply_operations(d, list(op)))
  }
})

test_that("valores con comillas no rompen el codigo", {
  expect_equal(format_value_for_code('dice "hola"', FALSE), '"dice \\"hola\\""')
  d <- data.frame(x = c('dice "hola"', "otro"), stringsAsFactors = FALSE)
  res <- apply_operations(d, list(op_filter("x", "==", 'dice "hola"', numeric = FALSE)))
  expect_equal(nrow(res), 1)
  # numeric = TRUE con un valor que no es numero: se escribe como texto
  expect_equal(format_value_for_code("abc", TRUE), '"abc"')
})

test_that("recodificar texto con ceros a la izquierda: codigo correcto", {
  d <- df_codigos()
  op <- op_recode("division", list("01" = "Alimentos", "02" = "Bebidas"), numeric = FALSE)
  res <- apply_operations(d, list(op))
  expect_equal(res$division, c("Alimentos", "Bebidas", "Alimentos", "10"))
  code <- code_recode(op$params)
  expect_match(code, 'division == "01" ~ "Alimentos"', fixed = TRUE)
  expect_equal(eval(parse(text = paste("d |>", code))), res)
})

test_that("recodificar a un valor que parece numero devuelve texto en el codigo", {
  d <- data.frame(r = c("Norte", "Sur"), stringsAsFactors = FALSE)
  op <- op_recode("r", list(Norte = "1"), numeric = FALSE)
  code <- code_recode(op$params)
  expect_match(code, 'r == "Norte" ~ "1"', fixed = TRUE)
  expect_equal(eval(parse(text = paste("d |>", code))), apply_operations(d, list(op)))
})

test_that("recodificar una columna numerica mantiene el valor original sin comillas", {
  op <- op_recode("n", list("1" = "uno"), numeric = TRUE)
  expect_match(code_recode(op$params), 'n == 1 ~ "uno"', fixed = TRUE)
})

test_that("el modulo Constructor registra el tipo de la columna al filtrar", {
  ds <- shiny::reactiveVal(df_codigos())
  nm <- shiny::reactiveVal("d")
  stack <- shiny::reactiveVal(list())
  code <- shiny::reactiveVal("")
  hist <- shiny::reactiveVal(list())
  shiny::testServer(mod_builder_server,
    args = list(active_dataset = ds, active_name = nm, operation_stack = stack,
                generated_code = code, history = hist), {
      session$setInputs(op_filter = 1, filter_col = "division", filter_op = "==",
                        filter_val = "01", confirm_filter = 1)
      expect_false(stack()[[1]]$params$numeric)
      expect_equal(nrow(preview_data()), 2)
      expect_match(code(), 'dplyr::filter(division == "01")', fixed = TRUE)
    })
})
