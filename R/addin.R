#' Lanzar RVisual
#'
#' Abre la interfaz visual de RVisual como gadget de RStudio o en el browser.
#'
#' @param browser Lógico. Si TRUE abre en el browser del sistema (necesario
#'   para que el Asistente IA pueda hacer requests HTTP). Default FALSE.
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
