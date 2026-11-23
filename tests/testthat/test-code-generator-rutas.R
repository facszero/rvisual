# Rutas de Windows en el paso de carga del codigo generado

ruta_win <- "C:\\Users\\fcanete\\AppData\\Local\\Temp\\RtmpX\\datos.rds"

test_that("una ruta de Windows produce un script que se puede leer (archivo)", {
  code <- generate_load_step("d", list(kind = "archivo", ext = "rds", filename = ruta_win))
  expect_silent(parse(text = code))
  expect_match(code, "C:/Users/fcanete/AppData/Local/Temp/RtmpX/datos.rds", fixed = TRUE)
})

test_that("una ruta de Windows produce un script que se puede leer (SQLite)", {
  code <- generate_load_step("d", list(kind = "db", tipo = "sqlite",
                                       filepath = sub("datos.rds", "base.sqlite", ruta_win,
                                                      fixed = TRUE),
                                       tabla = "personas"))
  expect_silent(parse(text = code))
  expect_match(code, "C:/Users/fcanete/", fixed = TRUE)
})

test_that("las rutas con barras normales no cambian", {
  expect_identical(ruta_para_codigo("datos/ech.csv"), "datos/ech.csv")
  expect_identical(ruta_para_codigo("ech.csv"), "ech.csv")
})
