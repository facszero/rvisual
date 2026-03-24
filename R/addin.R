#' Lanzar RVisual
#'
#' Abre la interfaz visual de RVisual como gadget integrado en RStudio
#' o en el browser del sistema.
#'
#' RVisual es un addin para RStudio que permite a usuarios con experiencia
#' en SPSS trabajar con datos en R sin necesidad de escribir c\u00f3digo
#' manualmente. Toda acci\u00f3n visual genera c\u00f3digo R limpio y reproducible
#' usando tidyverse.
#'
#' @param browser L\u00f3gico. Si \code{TRUE} abre en el browser del sistema
#'   en lugar del panel Viewer de RStudio. Recomendado cuando se usa el
#'   Asistente IA, ya que el panel embebido bloquea conexiones HTTP
#'   salientes. Default \code{FALSE}.
#'
#' @return Invisiblemente \code{NULL}. La funci\u00f3n bloquea la sesi\u00f3n R
#'   mientras la interfaz est\u00e1 abierta (comportamiento normal de gadgets
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

  # Subir l\u00edmite de upload a 50 MB
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
