#' Integraci\u00f3n con RStudio
#'
#' Funciones para interactuar con el IDE de RStudio desde el addin:
#' insertar c\u00f3digo, leer el script activo, ejecutar en la consola.
#'
#' @name rstudio_bridge
#' @keywords internal
NULL

#' Insertar c\u00f3digo en el script activo de RStudio
#'
#' @param code String con el c\u00f3digo R a insertar en la posici\u00f3n del cursor.
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

#' Ejecutar c\u00f3digo en la consola de RStudio
#' @param code String con el c\u00f3digo R a ejecutar.
#' @return Invisiblemente \code{NULL}.
#' @keywords internal
rstudio_run_in_console <- function(code) {
  if (rstudioapi::isAvailable()) rstudioapi::sendToConsole(code, execute = TRUE)
  else eval(parse(text = code), envir = .GlobalEnv)
}
