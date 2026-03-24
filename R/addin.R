#' Lanzar RVisual
#'
#' Abre la interfaz visual de RVisual como gadget integrado en RStudio
#' o en el browser del sistema.
#'
#' RVisual es un addin para RStudio que permite a usuarios con experiencia
#' en SPSS trabajar con datos en R sin necesidad de escribir código
#' manualmente. Toda acción visual genera código R limpio y reproducible
#' usando tidyverse.
#'
#' @param browser Lógico. Si \code{TRUE} abre en el browser del sistema
#'   en lugar del panel Viewer de RStudio. Recomendado cuando se usa el
#'   Asistente IA, ya que el panel embebido bloquea conexiones HTTP
#'   salientes. Default \code{FALSE}.
#'
#' @return Invisiblemente \code{NULL}. La función bloquea la sesión R
#'   mientras la interfaz está abierta (comportamiento normal de gadgets
#'   Shiny).
#'
#' @examples
#' \dontrun{
#' # Abrir en panel Viewer de RStudio
#' launch_rvisual()
#'
#' # Abrir en browser (necesario para el Asistente IA)
#' launch_rvisual(browser = TRUE)
#' }
#'
#' @export
#' @importFrom shiny runGadget runApp shinyApp dialogViewer
#' @importFrom rstudioapi isAvailable
launch_rvisual <- function(browser = FALSE) {

  # Subir límite de upload a 50 MB
  options(shiny.maxRequestSize = 50 * 1024^2)

  ui     <- rvisual_ui()
  server <- rvisual_server

  if (browser || !rstudioapi::isAvailable()) {
    shiny::runApp(
      shiny::shinyApp(ui = ui, server = server),
      launch.browser = TRUE
    )
  } else {
    viewer <- shiny::dialogViewer(
      dialogName = "RVisual",
      width  = 1200,
      height = 800
    )
    shiny::runGadget(
      app          = shiny::shinyApp(ui = ui, server = server),
      viewer       = viewer,
      stopOnCancel = TRUE
    )
  }
}
