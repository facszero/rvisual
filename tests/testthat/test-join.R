# tests/testthat/test-join.R
# Join del Constructor: busqueda de datasets, claves de distinto tipo,
# seleccion de columnas y paso de carga del segundo dataset.

izq <- function() {
  data.frame(id = c("001", "002", "003"), mes = c(1L, 1L, 2L),
             x = c(10, 20, 30), comun = c("a", "b", "c"),
             stringsAsFactors = FALSE)
}
der <- function() {
  data.frame(id = c(1, 2, 4), mes = c(1L, 1L, 1L),
             precio = c(100, 200, 400), comun = c("A", "B", "D"),
             otra = c(TRUE, FALSE, TRUE), stringsAsFactors = FALSE)
}

limpiar <- function(nm) {
  if (exists(nm, envir = rvisual_env(), inherits = FALSE))
    rm(list = nm, envir = rvisual_env())
  rvisual_set_sources(list())
}

test_that("rvisual_find_dataset encuentra datasets cargados en RVisual", {
  assign("tj_der", der(), envir = rvisual_env())
  on.exit(limpiar("tj_der"))
  expect_equal(rvisual_find_dataset("tj_der"), der())
  expect_error(rvisual_find_dataset("tj_no_existe_xyz"), "No se encontr")
})

test_that("rvisual_join_candidates agrupa cargados y entorno global", {
  assign("tj_der", der(), envir = rvisual_env())
  on.exit(limpiar("tj_der"))
  cand <- rvisual_join_candidates()
  expect_true("Cargados en RVisual" %in% names(cand))
  expect_true("tj_der" %in% unlist(cand[["Cargados en RVisual"]]))
  # cada grupo es una lista (un grupo con un solo elemento no se
  # confunde con una opcion suelta en selectInput)
  expect_true(is.list(cand[["Cargados en RVisual"]]))
})

test_that("un join viejo (sin parametros nuevos) se comporta igual", {
  assign("tj_der", der(), envir = rvisual_env())
  on.exit(limpiar("tj_der"))
  l <- izq(); l$id <- c(1, 2, 3)
  op <- op_join("tj_der", c(id = "id", mes = "mes"), "left")
  res <- apply_operations(l, list(op))
  esperado <- dplyr::left_join(l, der(), by = c(id = "id", mes = "mes"))
  expect_equal(res, esperado)
  expect_equal(code_join(op$params),
               'dplyr::left_join(tj_der, by = c("id", "mes"))')
})

test_that("join_key_check propone convertir texto numerico a numero", {
  chk <- join_key_check(izq(), der(), c(id = "id", mes = "mes"))
  expect_true(chk$ok)
  expect_length(chk$casts, 1)
  expect_equal(chk$casts[[1]], list(side = "left", col = "id"))
  # la conversion puede ir del lado derecho
  chk2 <- join_key_check(der(), izq(), c(id = "id"))
  expect_equal(chk2$casts[[1]], list(side = "right", col = "id"))
})

test_that("join_key_check rechaza texto no numerico contra numero", {
  l <- izq(); l$id <- c("A1", "002", "003")
  chk <- join_key_check(l, der(), c(id = "id"))
  expect_false(chk$ok)
  expect_length(chk$casts, 0)
  expect_match(chk$messages, "no se pueden unir")
  chk2 <- join_key_check(izq(), der(), c(id = "no_esta"))
  expect_false(chk2$ok)
})

test_that("join_key_check acepta tipos iguales y texto contra factor", {
  l <- izq(); r <- der(); r$id <- factor(c("001", "002", "004"))
  chk <- join_key_check(l, r, c(id = "id", mes = "mes"))
  expect_true(chk$ok)
  expect_length(chk$messages, 0)
})

test_that("el join convierte la clave y trae solo las columnas elegidas", {
  assign("tj_der", der(), envir = rvisual_env())
  on.exit(limpiar("tj_der"))
  by  <- c(id = "id", mes = "mes")
  chk <- join_key_check(izq(), der(), by)
  op  <- op_join("tj_der", by, "left", right_cols = "precio", key_casts = chk$casts)
  res <- apply_operations(izq(), list(op))
  expect_equal(names(res), c("id", "mes", "x", "comun", "precio"))
  expect_equal(res$precio, c(100, 200, NA))
  expect_true(is.numeric(res$id))
})

test_that("el codigo del join reproduce lo que hace la vista previa", {
  df_der <- der()
  by  <- c(id = "id", mes = "mes")
  chk <- join_key_check(izq(), df_der, by)
  op  <- op_join("df_der", by, "inner", right_cols = c("precio", "comun"),
                 key_casts = chk$casts)
  code <- code_join(op$params)
  expect_match(code, "dplyr::mutate(id = as.numeric(as.character(id)))", fixed = TRUE)
  expect_match(code, "df_der |> dplyr::select(id, mes, precio, comun)", fixed = TRUE)
  env <- new.env()
  env$df_der <- df_der
  env$df_izq <- izq()
  res_codigo <- eval(parse(text = paste0("df_izq |>\n  ", code)), envir = env)
  assign("df_der", df_der, envir = rvisual_env())
  on.exit(limpiar("df_der"))
  expect_equal(res_codigo, apply_operations(izq(), list(op)))
})

test_that("la conversion del lado derecho tambien se genera", {
  by  <- c(id = "id")
  chk <- join_key_check(der(), izq(), by)
  op  <- op_join("tj_izq", by, "left", key_casts = chk$casts)
  code <- code_join(op$params)
  expect_equal(code, paste0('dplyr::left_join(tj_izq |> dplyr::mutate(',
                            'id = as.numeric(as.character(id))), by = "id")'))
})

test_that("el codigo incluye el paso de carga del segundo dataset", {
  src <- list(kind = "archivo", ext = "rds", filename = "panel.rds")
  op  <- op_join("panel", c(id = "id"), "left", right_source = src)
  code <- generate_code("pares", list(op),
                        source = list(kind = "archivo", ext = "rds",
                                      filename = "pares.rds"))
  expect_match(code, 'pares <- readRDS("pares.rds")', fixed = TRUE)
  expect_match(code, 'panel <- readRDS("panel.rds")', fixed = TRUE)
  expect_lt(regexpr("panel <- readRDS", code), regexpr("left_join", code))
  # dos joins con el mismo dataset: una sola carga
  code2 <- generate_code("pares", list(op, op))
  expect_equal(lengths(regmatches(code2, gregexpr("panel <- readRDS", code2))), 1)
  # sin origen registrado (dataset del entorno): no se agrega carga
  op_viejo <- op_join("panel", c(id = "id"), "left")
  expect_equal(generate_join_load_steps(list(op_viejo)), character(0))
})

test_that("rvisual_source devuelve el origen registrado", {
  rvisual_set_sources(list(a = list(kind = "entorno")))
  on.exit(rvisual_set_sources(list()))
  expect_equal(rvisual_source("a"), list(kind = "entorno"))
  expect_null(rvisual_source("b"))
  expect_null(rvisual_source(NULL))
})

test_that("el modulo Constructor arma el join con datasets cargados en RVisual", {
  assign("tj_der", der(), envir = rvisual_env())
  rvisual_set_sources(list(tj_der = list(kind = "archivo", ext = "rds",
                                         filename = "tj_der.rds")))
  on.exit(limpiar("tj_der"))
  ds    <- shiny::reactiveVal(izq())
  nm    <- shiny::reactiveVal("tj_izq")
  stack <- shiny::reactiveVal(list())
  code  <- shiny::reactiveVal("")
  hist  <- shiny::reactiveVal(list())
  shiny::testServer(mod_builder_server,
    args = list(active_dataset = ds, active_name = nm,
                operation_stack = stack, generated_code = code,
                history = hist), {
      session$setInputs(op_join = 1)
      session$setInputs(join_right_df = "tj_der", join_type = "left",
                        join_col_left = "id", join_col_right = "id",
                        join_right_cols = c("precio", "otra"))
      chk <- join_check()
      expect_true(chk$ok)
      expect_length(chk$casts, 1)
      session$setInputs(confirm_join = 1)
      ops <- stack()
      expect_length(ops, 1)
      p <- ops[[1]]$params
      expect_equal(p$right_cols, c("precio", "otra"))
      expect_equal(p$right_source$filename, "tj_der.rds")
      res <- preview_data()
      expect_equal(names(res), c("id", "mes", "x", "comun", "precio", "otra"))
      expect_match(code(), 'tj_der <- readRDS("tj_der.rds")', fixed = TRUE)

      # claves incompatibles: no se apila la operacion
      session$setInputs(join_col_left = "comun", join_col_right = "precio")
      expect_false(join_check()$ok)
      session$setInputs(confirm_join = 2)
      expect_length(stack(), 1)
    })
})

test_that("los data.frames del entorno global siguen disponibles para unir", {
  global <- new.env()   # hace de entorno global, sin tocar el real
  assign("tj_global_df", der(), envir = global)
  expect_equal(rvisual_find_dataset("tj_global_df", global = global), der())
  cand <- rvisual_join_candidates(global = global)
  expect_true("tj_global_df" %in% unlist(cand[["Entorno global de R"]]))
  expect_error(rvisual_find_dataset("tj_global_df"), "No se encontr")
})

test_that("'Ninguna' trae solo las claves de la tabla derecha", {
  assign("tj_der", der(), envir = rvisual_env())
  on.exit(limpiar("tj_der"))
  ds    <- shiny::reactiveVal(izq())
  stack <- shiny::reactiveVal(list())
  shiny::testServer(mod_builder_server,
    args = list(active_dataset = ds, active_name = shiny::reactiveVal("tj_izq"),
                operation_stack = stack, generated_code = shiny::reactiveVal(""),
                history = shiny::reactiveVal(list())), {
      session$setInputs(op_join = 1)
      session$setInputs(join_right_df = "tj_der", join_type = "left",
                        join_col_left = "id", join_col_right = "id")
      session$setInputs(join_cols_none = 1)
      expect_true(rv_join$none)
      session$setInputs(join_right_cols = NULL, confirm_join = 1)
      p <- stack()[[1]]$params
      expect_identical(p$right_cols, character(0))
      expect_match(code_join(p), "tj_der |> dplyr::select(id)", fixed = TRUE)
    })
})
