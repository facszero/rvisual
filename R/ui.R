#' UI principal de RVisual
#'
#' Define la interfaz Shiny completa del addin.
#' Estructura en 6 paneles navegables via sidebar.
#'
#' @return Un objeto shiny.tag con la UI completa
#' @import shiny bslib htmltools
rvisual_ui <- function() {
  bslib::page_navbar(
    title = shiny::tags$span(
      shiny::tags$img(src = "rvisual_logo.png", height = "24px", style = "margin-right:8px;"),
      "RVisual"
    ),
    theme = bslib::bs_theme(
      version    = 5,
      bootswatch = "flatly",
      primary    = "#2C6FAC"
    ),
    header = shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "custom.css")
    ),
    window_title = "RVisual",

    # ── Panel 1: Dataset ──────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("database"), " Dataset"),
      value = "panel_dataset",
      mod_dataset_ui("dataset")
    ),

    # ── Panel 2: Explorador ───────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("table"), " Explorador"),
      value = "panel_explorer",
      mod_explorer_ui("explorer")
    ),

    # ── Panel 3: Constructor Visual ───────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("sliders"), " Constructor"),
      value = "panel_builder",
      mod_builder_ui("builder")
    ),

    # ── Panel 4: Código R ─────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("code"), " Código R"),
      value = "panel_code",
      mod_code_ui("code")
    ),

    # ── Panel 5: Asistente IA ─────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("robot"), " Asistente IA"),
      value = "panel_ai",
      mod_ai_ui("ai")
    ),

    # ── Panel 6: Configuración ────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("gear"), " Configuración"),
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
