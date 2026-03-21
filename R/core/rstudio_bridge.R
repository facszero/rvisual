#' RStudio Bridge — integración con el IDE
#'
#' Funciones para interactuar con RStudio desde el addin:
#' insertar código, leer entorno, navegar entre archivos.

#' Insertar código en el script activo de RStudio
#'
#' @param code String con el código R a insertar
#' @export
rstudio_insert_code <- function(code) {
  if (rstudioapi::isAvailable()) {
    rstudioapi::insertText(text = paste0("\n", code, "\n"))
  } else {
    warning("RStudio API no disponible. No se puede insertar código.")
  }
}

#' Obtener el path del script activo en RStudio
#' @export
rstudio_active_script_path <- function() {
  if (rstudioapi::isAvailable()) {
    ctx <- rstudioapi::getActiveDocumentContext()
    ctx$path
  } else {
    NULL
  }
}

#' Ejecutar código en la consola de RStudio
#' @export
rstudio_run_in_console <- function(code) {
  if (rstudioapi::isAvailable()) {
    rstudioapi::sendToConsole(code, execute = TRUE)
  } else {
    eval(parse(text = code), envir = .GlobalEnv)
  }
}
