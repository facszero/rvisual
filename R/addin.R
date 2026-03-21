#' Lanzar RVisual
#'
#' Punto de entrada del addin de RStudio. Abre la interfaz visual de RVisual
#' como un gadget de Shiny integrado en el panel Viewer de RStudio.
#'
#' @export
launch_rvisual <- function() {
  # Verificar que estamos dentro de RStudio
  if (!rstudioapi::isAvailable()) {
    message("RVisual requiere RStudio para funcionar correctamente.")
    message("Abriendo en modo browser como alternativa...")
  }

  ui <- rvisual_ui()
  server <- rvisual_server

  viewer <- shiny::dialogViewer(
    dialogName = "RVisual",
    width = 1200,
    height = 800
  )

  shiny::runGadget(
    app    = shiny::shinyApp(ui = ui, server = server),
    viewer = viewer,
    stopOnCancel = TRUE
  )
}
