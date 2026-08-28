#' Integracion con RStudio
#'
#' Funciones para interactuar con el IDE de RStudio desde el addin:
#' insertar codigo, leer el script activo, ejecutar en la consola.
#'
#' @name rstudio_bridge
#' @keywords internal
NULL

#' Insertar codigo en el script activo de RStudio
#'
#' @param code String con el codigo R a insertar en la posicion del cursor.
#' @return Invisiblemente \code{NULL}.
#' @keywords internal
rstudio_insert_code <- function(code) {
  if (rstudioapi::isAvailable()) {
    rstudioapi::insertText(text = paste0("\n", code, "\n"))
  } else {
    warning("RStudio API no disponible. No se puede insertar c\u00f3digo.")
  }
}

#' Obtener el path del script activo en RStudio
#' @return String con la ruta del documento activo, o \code{NULL}.
#' @keywords internal
rstudio_active_script_path <- function() {
  if (rstudioapi::isAvailable()) rstudioapi::getActiveDocumentContext()$path
  else NULL
}

#' Ejecutar codigo en la consola de RStudio
#' @param code String con el codigo R a ejecutar.
#' @return Invisiblemente \code{NULL}.
#' @keywords internal
rstudio_run_in_console <- function(code) {
  if (rstudioapi::isAvailable()) rstudioapi::sendToConsole(code, execute = TRUE)
  else eval(parse(text = code), envir = .GlobalEnv)
}
