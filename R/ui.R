#' UI principal de RVisual
#'
#' Define la interfaz Shiny completa del addin.
#' Estructura en 6 paneles navegables via sidebar.
#'
#' @return Un objeto shiny.tag con la UI completa
#' @import shiny bslib htmltools
rvisual_ui <- function() {
  rvisual_add_resource_path()
  bslib::page_navbar(
    id = "main_nav",
    title = shiny::tags$span(
      shiny::tags$img(src = "rvisual-www/rvisual_logo.svg", height = "26px",
                      alt = "", style = "margin-right:8px; vertical-align:-6px;"),
      "RVisual"
    ),
    theme = bslib::bs_theme(
      version    = 5,
      bootswatch = "flatly",
      primary    = "#2C6FAC"
    ),
    header = shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "rvisual-www/custom.css")
    ),
    window_title = "RVisual",

    # -- Panel 1: Dataset --------------------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("database"), " Dataset"),
      value = "panel_dataset",
      mod_dataset_ui("dataset")
    ),

    # -- Panel 2: Explorador -----------------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("table"), " Explorador"),
      value = "panel_explorer",
      mod_explorer_ui("explorer")
    ),

    # -- Panel 3: Constructor Visual ---------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("sliders"), " Constructor"),
      value = "panel_builder",
      mod_builder_ui("builder")
    ),

    # -- Panel 4: C\u00f3digo R -------------------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("code"), " C\u00f3digo R"),
      value = "panel_code",
      mod_code_ui("code")
    ),

    # -- Panel 5: Asistente IA ---------------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("robot"), " Asistente IA"),
      value = "panel_ai",
      mod_ai_ui("ai")
    ),

    # -- Panel 6: Configuraci\u00f3n --------------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("gear"), " Configuraci\u00f3n"),
      value = "panel_config",
      mod_config_ui("config")
    ),

    # Historial siempre visible como panel colapsable inferior (futuro)
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::actionLink("btn_history", label = shiny::icon("clock-rotate-left"),
                        title = "Ver historial de operaciones")
    )
  )
}

#' Publicar los recursos estaticos del paquete (inst/www)
#'
#' Dentro de un paquete, Shiny no sirve \code{inst/www} automaticamente:
#' sin este registro ni el logo ni \code{custom.css} llegan al navegador.
#'
#' @return Invisiblemente, la ruta registrada.
#' @keywords internal
rvisual_add_resource_path <- function() {
  www <- system.file("www", package = "rvisual")
  if (nzchar(www)) shiny::addResourcePath("rvisual-www", www)
  invisible(www)
}
