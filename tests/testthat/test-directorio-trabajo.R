# Boton "Cambiar..." del directorio de trabajo (pestania Dataset)

args_dataset <- function() {
  list(active_dataset = shiny::reactiveVal(NULL), active_name = shiny::reactiveVal(NULL),
       history = shiny::reactiveVal(list()), active_source = shiny::reactiveVal(NULL))
}

test_that("el selector de RStudio cambia el directorio de trabajo", {
  viejo <- getwd()
  on.exit(setwd(viejo))
  destino <- normalizePath(tempdir(), winslash = "/")
  testthat::local_mocked_bindings(rvisual_select_directory = function(path) destino)
  shiny::testServer(mod_dataset_server, args = args_dataset(), {
    session$setInputs(btn_wd = 1)
    expect_identical(normalizePath(getwd(), winslash = "/"), destino)
    expect_identical(wd(), getwd())
  })
})

test_that("cancelar el selector no cambia nada", {
  viejo <- getwd()
  on.exit(setwd(viejo))
  testthat::local_mocked_bindings(rvisual_select_directory = function(path) NULL)
  shiny::testServer(mod_dataset_server, args = args_dataset(), {
    session$setInputs(btn_wd = 1)
    expect_identical(getwd(), viejo)
  })
})

test_that("fuera de RStudio se escribe la ruta (con barras invertidas o no)", {
  viejo <- getwd()
  on.exit(setwd(viejo))
  destino <- normalizePath(tempdir(), winslash = "/")
  testthat::local_mocked_bindings(rvisual_select_directory = function(path) NA)
  shiny::testServer(mod_dataset_server, args = args_dataset(), {
    session$setInputs(btn_wd = 1)
    expect_identical(getwd(), viejo)          # todavia no cambio: abre el dialogo
    session$setInputs(wd_text = "C:/no/existe/xyz", confirm_wd = 1)
    expect_identical(getwd(), viejo)          # carpeta inexistente: no cambia
    session$setInputs(wd_text = gsub("/", "\\\\", destino, fixed = TRUE), confirm_wd = 2)
    expect_identical(normalizePath(getwd(), winslash = "/"), destino)
  })
})

test_that("el panel avisa si el archivo del dataset activo esta en el directorio", {
  viejo <- getwd()
  on.exit(setwd(viejo))
  dir <- tempfile("wd")
  dir.create(dir)
  file.create(file.path(dir, "datos.rds"))
  setwd(dir)
  ds   <- shiny::reactiveVal(NULL)
  nm   <- shiny::reactiveVal(NULL)
  src  <- shiny::reactiveVal(list(kind = "archivo", ext = "rds", filename = "datos.rds"))
  hist <- shiny::reactiveVal(list())
  shiny::testServer(mod_dataset_server,
    args = list(active_dataset = ds, active_name = nm, history = hist,
                active_source = src), {
      expect_match(as.character(output$wd_ui$html), "lo va a encontrar")
      src(list(kind = "archivo", ext = "rds", filename = "otro.rds"))
      session$flushReact()
      expect_match(as.character(output$wd_ui$html), "no est")
    })
})

test_that("rvisual_select_directory devuelve NA fuera de RStudio", {
  skip_if(rstudioapi::isAvailable())
  expect_identical(rvisual_select_directory(), NA)
})
