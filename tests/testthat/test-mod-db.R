# Tests del panel de base de datos (mod_db_server)

db_args <- function() {
  list(active_dataset = shiny::reactiveVal(NULL),
       active_name    = shiny::reactiveVal(NULL),
       datasets_rv    = shiny::reactiveVal(list()))
}

test_that("elegir SQLite como motor no corta la sesion", {
  withr::local_envvar(R_USER_CONFIG_DIR = tempfile("cfg"))
  shiny::testServer(mod_db_server, args = db_args(), {
    expect_no_error(session$setInputs(tipo = "sqlite"))
    expect_no_error(session$setInputs(tipo = "postgresql"))
  })
})

test_that("conectar a SQLite lista las tablas y al cerrar se desconecta", {
  skip_if_not_installed("RSQLite")
  withr::local_envvar(R_USER_CONFIG_DIR = tempfile("cfg"))
  copia <- tempfile(fileext = ".sqlite")
  con_ini <- DBI::dbConnect(RSQLite::SQLite(), copia)
  DBI::dbWriteTable(con_ini, "personas", data.frame(id = 1:3, edad = c(20, 35, 50)))
  DBI::dbWriteTable(con_ini, "departamentos", data.frame(departamento = 1:2))
  DBI::dbDisconnect(con_ini)

  con <- NULL
  shiny::testServer(mod_db_server, args = db_args(), {
    session$setInputs(tipo = "sqlite",
                      sqlite_file = data.frame(name = "prueba.sqlite", size = 1,
                                               type = "", datapath = copia))
    session$setInputs(btn_conectar = 1)
    expect_setequal(tablas_list(), c("personas", "departamentos"))
    con <<- con_activa()
    expect_true(DBI::dbIsValid(con))
    expect_no_error(session$close())
  })
  expect_false(DBI::dbIsValid(con))
})

test_that("cerrar la sesion sin conexion activa no falla", {
  shiny::testServer(mod_db_server, args = db_args(), {
    expect_no_error(session$close())
  })
})
