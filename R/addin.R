#' Lanzar RVisual
#'
#' Abre la interfaz visual de RVisual como gadget integrado en RStudio
#' o en el browser del sistema.
#'
#' RVisual es un addin para RStudio que permite a usuarios con experiencia
#' en SPSS trabajar con datos en R sin necesidad de escribir codigo
#' manualmente. Toda accion visual genera codigo R limpio y reproducible
#' usando tidyverse.
#'
#' @param browser Logico. Si \code{TRUE} abre en el browser del sistema
#'   en lugar del panel Viewer de RStudio. Recomendado cuando se usa el
#'   Asistente IA, ya que el panel embebido bloquea conexiones HTTP
#'   salientes. Default \code{FALSE}.
#'
#' @param width,height Tamano en pixeles de la ventana del addin dentro de
#'   RStudio. Por defecto 1400 x 900, o los valores de las opciones
#'   \code{rvisual.width} y \code{rvisual.height} si estan definidas (por
#'   ejemplo en \code{.Rprofile}). Se ignoran si se abre en el navegador.
#'
#' @return Invisiblemente \code{NULL}. La funcion bloquea la sesion R
#'   mientras la interfaz esta abierta (comportamiento normal de gadgets
#'   Shiny).
#'
#' @examples
#' \dontrun{
#' # Abrir en panel Viewer de RStudio
#' launch_rvisual()
#'
#' # Abrir en browser (necesario para el Asistente IA)
#' launch_rvisual(browser = TRUE)
#'
#' # Ventana mas grande dentro de RStudio
#' launch_rvisual(width = 1800, height = 1000)
#'
#' # Fijar el tamano para siempre (por ejemplo en .Rprofile)
#' options(rvisual.width = 1800, rvisual.height = 1000)
#' }
#'
#' @export
#' @importFrom shiny runGadget runApp shinyApp dialogViewer
#' @importFrom rstudioapi isAvailable
launch_rvisual <- function(browser = FALSE,
                           width  = getOption("rvisual.width", 1400),
                           height = getOption("rvisual.height", 900)) {

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
      width  = width,
      height = height
    )
    shiny::runGadget(
      app          = shiny::shinyApp(ui = ui, server = server),
      viewer       = viewer,
      stopOnCancel = TRUE
    )
  }
}
