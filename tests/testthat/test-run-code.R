# Ejecucion del codigo generado dentro del addin (panel Codigo R y chat)

test_that("un dataset que solo esta en RVisual se encuentra al ejecutar", {
  assign("tj_solo_rvisual", data.frame(x = 1:3), envir = rvisual_env())
  on.exit(rm("tj_solo_rvisual", envir = rvisual_env()))
  res <- rvisual_eval_code("tj_solo_rvisual |> dplyr::filter(x > 1)")
  expect_equal(res$value$x, 2:3)
  expect_identical(res$from_rvisual, "tj_solo_rvisual")
  # no se escribio nada en el entorno global
  expect_false(exists("tj_solo_rvisual", envir = globalenv(), inherits = FALSE))
})

test_that("si el codigo crea el dataset o esta en el global, no se usa la copia", {
  assign("tj_x", data.frame(x = 1:3), envir = rvisual_env())
  on.exit(rm("tj_x", envir = rvisual_env()))
  expect_identical(rvisual_code_needs(parse(text = "tj_x |> head()")), "tj_x")
  # el codigo lo crea (paso de carga)
  expect_length(rvisual_code_needs(parse(text = "tj_x <- readRDS('a.rds')\ntj_x |> head()")), 0)
  # ya existe en el entorno global del usuario
  global <- new.env()
  assign("tj_x", 1, envir = global)
  expect_length(rvisual_code_needs(parse(text = "tj_x |> head()"), global = global), 0)
})

test_that("codigo sin datasets de RVisual se evalua normalmente", {
  res <- rvisual_eval_code("1 + 1")
  expect_identical(res$value, 2)
  expect_length(res$from_rvisual, 0)
})
