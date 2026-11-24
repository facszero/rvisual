# Los dialogos del Constructor ofrecen las columnas del resultado actual
# (dataset activo + operaciones ya apiladas), no solo las del original.

base_cc <- function() {
  data.frame(div = c("01", "01", "02"), p = c(10, 12, 20), q = c(11, 12, 18),
             stringsAsFactors = FALSE)
}

test_that("una variable creada aparece para agrupar, filtrar y recodificar", {
  ds    <- shiny::reactiveVal(base_cc())
  nm    <- shiny::reactiveVal("d")
  stack <- shiny::reactiveVal(list(op_mutate("err", "abs(q / p - 1)")))
  code  <- shiny::reactiveVal("")
  hist  <- shiny::reactiveVal(list())
  shiny::testServer(mod_builder_server,
    args = list(active_dataset = ds, active_name = nm, operation_stack = stack,
                generated_code = code, history = hist), {
      expect_true("err" %in% cols())
      expect_true("err" %in% cols_num())

      # Agrupar y resumir con la variable creada
      session$setInputs(op_group = 1)
      grp_cols_selected("div")
      session$setInputs(sum_newcol_1 = "mediana_err", sum_fn_1 = "median",
                        sum_col_1 = "err", confirm_group = 1)
      res <- preview_data()
      expect_equal(names(res), c("div", "mediana_err"))
      expect_equal(res$mediana_err, c(median(abs(c(11, 12) / c(10, 12) - 1)), abs(18 / 20 - 1)))

      # despues de agrupar, las columnas ofrecidas son las del resumen
      expect_setequal(cols(), c("div", "mediana_err"))
    })
})

test_that("con el stack vacio las columnas son las del dataset activo", {
  ds    <- shiny::reactiveVal(base_cc())
  stack <- shiny::reactiveVal(list())
  shiny::testServer(mod_builder_server,
    args = list(active_dataset = ds, active_name = shiny::reactiveVal("d"),
                operation_stack = stack, generated_code = shiny::reactiveVal(""),
                history = shiny::reactiveVal(list())), {
      expect_identical(cols(), names(base_cc()))
      expect_identical(preview_data(), base_cc())
    })
})

test_that("si una operacion falla, la vista previa vuelve al dataset original", {
  ds    <- shiny::reactiveVal(base_cc())
  stack <- shiny::reactiveVal(list(op_select("no_existe")))
  shiny::testServer(mod_builder_server,
    args = list(active_dataset = ds, active_name = shiny::reactiveVal("d"),
                operation_stack = stack, generated_code = shiny::reactiveVal(""),
                history = shiny::reactiveVal(list())), {
      expect_identical(preview_data(), base_cc())
      expect_identical(cols(), names(base_cc()))
    })
})

test_that("filtrar una variable creada registra su tipo real", {
  ds    <- shiny::reactiveVal(base_cc())
  stack <- shiny::reactiveVal(list(op_mutate("cod", "paste0('0', round(p))")))
  code  <- shiny::reactiveVal("")
  shiny::testServer(mod_builder_server,
    args = list(active_dataset = ds, active_name = shiny::reactiveVal("d"),
                operation_stack = stack, generated_code = code,
                history = shiny::reactiveVal(list())), {
      session$setInputs(op_filter = 1, filter_col = "cod", filter_op = "==",
                        filter_val = "010", confirm_filter = 1)
      expect_false(stack()[[2]]$params$numeric)
      expect_equal(nrow(preview_data()), 1)
      expect_match(code(), 'dplyr::filter(cod == "010")', fixed = TRUE)
    })
})
