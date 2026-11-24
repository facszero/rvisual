# Recursos estaticos de la interfaz (logo y CSS) y tamano de la ventana

test_that("la carpeta www del paquete se publica con logo y CSS", {
  www <- rvisual_add_resource_path()
  expect_true(nzchar(www))
  expect_true(file.exists(file.path(www, "rvisual_logo.svg")))
  expect_true(file.exists(file.path(www, "custom.css")))
  expect_true("rvisual-www" %in% names(shiny::resourcePaths()))
})

test_that("la UI referencia el logo y el CSS por la ruta publicada", {
  r <- htmltools::renderTags(rvisual_ui())
  expect_match(r$html, "rvisual-www/rvisual_logo.svg", fixed = TRUE)
  expect_match(r$head, "rvisual-www/custom.css", fixed = TRUE)
})

test_that("el tamano de la ventana es configurable", {
  f <- formals(launch_rvisual)
  expect_true(all(c("width", "height") %in% names(f)))
  withr::local_options(rvisual.width = 1700, rvisual.height = 950)
  expect_identical(eval(f$width), 1700)
  expect_identical(eval(f$height), 950)
})
