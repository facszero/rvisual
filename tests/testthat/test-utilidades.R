# Utilidades internas

test_that("%||% devuelve el valor por defecto solo si falta el valor", {
  expect_identical(NULL %||% "x", "x")
  expect_identical(character(0) %||% "x", "x")
  expect_identical("" %||% "x", "x")
  expect_identical("a" %||% "x", "a")
  expect_identical(0 %||% 5, 0)
  expect_identical(NA %||% 5, NA)
  # vectores de largo > 1 (selectize multiple, checkboxGroup)
  expect_identical(c("a", "b") %||% "x", c("a", "b"))
})

