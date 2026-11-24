# Claves de API en el llavero del sistema y archivo de configuracion

test_that("con llavero, la clave no queda en el archivo y se recupera al cargar", {
  ruta <- tempfile(fileext = ".json")
  llavero <- new.env()
  testthat::local_mocked_bindings(
    config_path = function() ruta,
    rvisual_key_set = function(provider, key) { assign(provider, key, envir = llavero); TRUE },
    rvisual_key_get = function(provider) get0(provider, envir = llavero)
  )
  config_save(list(provider = "anthropic", model = "m", api_key = "sk-secreta"))
  expect_false(grepl("sk-secreta", paste(readLines(ruta), collapse = "")))
  cfg <- config_load()
  expect_identical(cfg$api_key, "sk-secreta")
  expect_true(isTRUE(cfg$keyring_api))
})

test_that("sin llavero, la clave se guarda en el archivo como antes", {
  ruta <- tempfile(fileext = ".json")
  testthat::local_mocked_bindings(
    config_path = function() ruta,
    rvisual_key_set = function(provider, key) FALSE
  )
  config_save(list(provider = "openai", api_key = "sk-plana"))
  expect_true(grepl("sk-plana", paste(readLines(ruta), collapse = "")))
  expect_identical(config_load()$api_key, "sk-plana")
})

test_that("guardar la configuracion de IA conserva las conexiones de BD", {
  ruta <- tempfile(fileext = ".json")
  testthat::local_mocked_bindings(
    config_path = function() ruta,
    rvisual_key_set = function(provider, key) FALSE
  )
  config_save(list(db_last = list(sqlite = list(host = "h"))))
  cfg_ia <- shiny::reactiveVal(list())
  shiny::testServer(mod_config_server, args = list(ai_config = cfg_ia), {
    session$setInputs(ai_provider = "openai", ai_model = "gpt-4o-mini",
                      api_key = "sk-x", allow_sample = FALSE, allow_schema = TRUE,
                      btn_save_config = 1)
  })
  cfg <- config_load()
  expect_identical(cfg$db_last$sqlite$host, "h")
  expect_identical(cfg$provider, "openai")
})

test_that("el texto de la pantalla dice donde se guarda la clave", {
  testthat::local_mocked_bindings(rvisual_keyring_ok = function() TRUE)
  expect_match(rvisual_key_storage_label(), "llavero")
  testthat::local_mocked_bindings(rvisual_keyring_ok = function() FALSE,
                                  config_path = function() "/tmp/x.json")
  expect_match(rvisual_key_storage_label(), "texto plano")
})
