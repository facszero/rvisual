#' Lanzar RVisual
#'
#' Punto de entrada del addin de RStudio.
#' @export
launch_rvisual <- function(browser = FALSE) {

  # Subir límite de upload a 50 MB
  options(shiny.maxRequestSize = 50 * 1024^2)

  ui     <- rvisual_ui()
  server <- rvisual_server

  if (browser || !rstudioapi::isAvailable()) {
    # Abrir en browser externo (necesario para que IA pueda hacer HTTP)
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
