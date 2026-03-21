#' Configuración persistente de RVisual
#'
#' Guarda/lee configuración en un archivo JSON local
#' usando el directorio de datos del usuario (rappdirs-style).

config_path <- function() {
  dir <- tools::R_user_dir("rvisual", which = "config")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, "config.json")
}

#' Guardar configuración
#' @param cfg Lista de configuración
#' @export
config_save <- function(cfg) {
  # Nunca guardar la API key en texto plano si se puede evitar
  # TODO: integrar con keyring en Fase 6
  jsonlite::write_json(cfg, config_path(), auto_unbox = TRUE)
  invisible(cfg)
}

#' Cargar configuración guardada
#' @return Lista de configuración o lista vacía
#' @export
config_load <- function() {
  path <- config_path()
  if (!file.exists(path)) return(list())
  tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) list()
  )
}

# ── Historial de sesión ───────────────────────────────────────────────────

#' Registrar evento en el historial de sesión
#'
#' @param history reactiveVal que contiene la lista de eventos
#' @param type    String identificador del tipo de evento
#' @param detail  Lista con detalles del evento
#' @export
history_log <- function(history, type, detail = list()) {
  entry <- list(
    type      = type,
    detail    = detail,
    timestamp = Sys.time()
  )
  history(c(history(), list(entry)))
  invisible(entry)
}

#' Renderizar historial como UI inline (para modal)
#' @export
mod_history_ui_inline <- function(history_list) {
  if (length(history_list) == 0) {
    return(shiny::p("Sin eventos registrados.", class = "text-muted"))
  }

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
          shiny::tags$td(
            shiny::tags$small(
              paste(names(e$detail), unlist(e$detail), sep = "=", collapse = ", ")
            )
          )
        )
      })
    )
  )
}
