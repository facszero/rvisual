#' M\u00f3dulo: Configuraci\u00f3n
#'
#' Pantalla de configuraci\u00f3n del addin.
#' Permite gestionar proveedores IA, API keys, modelos y preferencias.

# -- UI --------------------------------------------------------------------
mod_config_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(6, 6),

    # Columna 1: Proveedor IA
    bslib::card(
      bslib::card_header(shiny::icon("robot"), " Proveedor de IA"),
      shiny::selectInput(
        ns("ai_provider"), "Proveedor:",
        choices  = c("OpenAI" = "openai", "Anthropic" = "anthropic", "Google Gemini" = "gemini"),
        selected = "anthropic"
      ),
      shiny::uiOutput(ns("model_selector")),
      shiny::passwordInput(ns("api_key"), "API Key:", placeholder = "sk-..."),
      shiny::div(
        class = "alert alert-info small p-2",
        shiny::icon("lock"), " Tu API key se guarda localmente y nunca se comparte."
      ),
      bslib::card_footer(
        shiny::actionButton(ns("btn_test_connection"), "Probar conexi\u00f3n",
                            icon = shiny::icon("plug"), class = "btn-outline-primary btn-sm"),
        shiny::actionButton(ns("btn_save_config"), "Guardar configuraci\u00f3n",
                            icon = shiny::icon("floppy-disk"), class = "btn-primary btn-sm ms-2")
      )
    ),

    # Columna 2: Privacidad y preferencias
    bslib::card(
      bslib::card_header(shiny::icon("shield"), " Privacidad y datos"),
      shiny::checkboxInput(ns("allow_sample"), "Permitir env\u00edo de muestra de datos a IA", value = FALSE),
      shiny::checkboxInput(ns("allow_schema"), "Enviar esquema del dataset (nombres y tipos)", value = TRUE),
      shiny::div(
        class = "alert alert-warning small p-2",
        shiny::icon("triangle-exclamation"),
        " Por defecto, solo se env\u00eda el esquema (nombres y tipos de columnas), nunca las filas."
      ),
      shiny::hr(),
      shiny::tags$h6("Preferencias de interfaz"),
      shiny::selectInput(ns("ui_language"), "Idioma:", choices = c("Espa\u00f1ol" = "es"), selected = "es"),
      shiny::checkboxInput(ns("show_code_live"), "Mostrar c\u00f3digo en tiempo real en panel Constructor", value = TRUE),
      bslib::card_footer(
        shiny::uiOutput(ns("config_status"))
      )
    )
  )
}

# -- Server ----------------------------------------------------------------
mod_config_server <- function(id, ai_config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Cargar config guardada al iniciar
    shiny::observe({
      cfg <- ai_config()
      if (!is.null(cfg)) {
        if (!is.null(cfg$provider))
          shiny::updateSelectInput(session, "ai_provider", selected = cfg$provider)
        if (!is.null(cfg$api_key) && nchar(cfg$api_key) > 0)
          shiny::updateTextInput(session, "api_key", value = cfg$api_key)
      }
    })

    # Selector de modelo seg\u00fan proveedor
    output$model_selector <- shiny::renderUI({
      models <- switch(input$ai_provider,
        "openai"    = c("gpt-4o", "gpt-4o-mini", "gpt-4-turbo"),
        "anthropic" = c("claude-opus-4-5", "claude-sonnet-4-5", "claude-haiku-4-5-20251001"),
        "gemini"    = c("gemini-1.5-pro", "gemini-1.5-flash"),
        c()
      )
      shiny::selectInput(ns("ai_model"), "Modelo:", choices = models)
    })

    # Guardar configuraci\u00f3n
    shiny::observeEvent(input$btn_save_config, {
      cfg <- list(
        provider     = input$ai_provider,
        model        = input$ai_model,
        api_key      = input$api_key,
        allow_sample = input$allow_sample,
        allow_schema = input$allow_schema
      )
      config_save(cfg)
      ai_config(cfg)
      shiny::showNotification("Configuraci\u00f3n guardada.", type = "message")
    })

    # Probar conexi\u00f3n
    shiny::observeEvent(input$btn_test_connection, {
      shiny::req(input$api_key, nchar(input$api_key) > 0)
      cfg <- list(provider = input$ai_provider, model = input$ai_model, api_key = input$api_key)
      result <- tryCatch(
        ai_test_connection(cfg),
        error = function(e) list(success = FALSE, message = e$message)
      )
      if (result$success) {
        shiny::showNotification("Conexi\u00f3n exitosa.", type = "message")
      } else {
        shiny::showNotification(paste("Error:", result$message), type = "error")
      }
    })

    output$config_status <- shiny::renderUI({
      cfg <- ai_config()
      if (!is.null(cfg$provider)) {
        shiny::div(class = "badge bg-success",
                   shiny::icon("check"), " ", cfg$provider, " configurado")
      } else {
        shiny::div(class = "badge bg-secondary", "Sin configurar")
      }
    })

  })
}
