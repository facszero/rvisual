#' Configuraci\u00f3n persistente y historial de sesi\u00f3n
#'
#' Funciones para guardar/cargar configuraci\u00f3n del usuario (proveedor IA,
#' API keys, preferencias) y registrar eventos durante la sesi\u00f3n.
#'
#' La configuraci\u00f3n se guarda en un archivo JSON en el directorio de datos
#' del usuario (ver \code{\link[tools]{R_user_dir}}).
#'
#' @name config
#' @keywords internal
NULL

config_path <- function() {
  dir <- tools::R_user_dir("rvisual", which = "config")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, "config.json")
}

#' Guardar configuraci\u00f3n del usuario
#' @param cfg Lista con los campos de configuraci\u00f3n.
#' @return Invisiblemente \code{cfg}.
#' @keywords internal
config_save <- function(cfg) {
  jsonlite::write_json(cfg, config_path(), auto_unbox = TRUE)
  invisible(cfg)
}

#' Cargar configuraci\u00f3n guardada
#' @return Lista de configuraci\u00f3n, o lista vac\u00eda si no existe.
#' @keywords internal
config_load <- function() {
  path <- config_path()
  if (!file.exists(path)) return(list())
  tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
           error = function(e) list())
}

#' Registrar un evento en el historial de sesi\u00f3n
#'
#' @param history \code{reactiveVal} que contiene la lista de eventos.
#' @param type String identificador del tipo de evento.
#' @param detail Lista con detalles adicionales del evento.
#' @return Invisiblemente el entry creado.
#' @keywords internal
history_log <- function(history, type, detail = list()) {
  entry <- list(type = type, detail = detail, timestamp = Sys.time())
  history(c(shiny::isolate(history()), list(entry)))
  invisible(entry)
}

#' Renderizar historial como tabla HTML (para modal)
#' @param history_list Lista de eventos del historial.
#' @return Objeto UI de Shiny.
#' @keywords internal
mod_history_ui_inline <- function(history_list) {
  if (length(history_list) == 0)
    return(shiny::p("Sin eventos registrados.", class = "text-muted"))

  shiny::tags$table(
    class = "table table-sm table-striped",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th("Hora"),
      shiny::tags$th("Evento"),
      shiny::tags$th("Detalle")
    )),
    shiny::tags$tbody(
      lapply(rev(history_list), function(e) {
        shiny::tags$tr(
          shiny::tags$td(format(e$timestamp, "%H:%M:%S")),
          shiny::tags$td(shiny::tags$code(e$type)),
          shiny::tags$td(shiny::tags$small(
            paste(names(e$detail), unlist(e$detail), sep = "=", collapse = ", ")
          ))
        )
      })
    )
  )
}
