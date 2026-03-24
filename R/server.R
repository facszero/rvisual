#' Server principal de RVisual
#'
#' Orquesta todos los m\u00f3dulos Shiny.
#' El estado compartido entre m\u00f3dulos se gestiona a trav\u00e9s de `reactives`
#' pasados como argumentos - no mediante variables globales.
#'
#' @param input,output,session Par\u00e1metros est\u00e1ndar de Shiny server
rvisual_server <- function(input, output, session) {

  # -- Estado global reactivo compartido entre m\u00f3dulos -------------------
  # Dataset activo seleccionado
  active_dataset <- shiny::reactiveVal(NULL)

  # Nombre del dataset activo
  active_dataset_name <- shiny::reactiveVal(NULL)

  # Lista de operaciones construidas visualmente (ver operation_model.R)
  operation_stack <- shiny::reactiveVal(list())

  # C\u00f3digo R generado a partir del operation_stack
  generated_code <- shiny::reactiveVal("")

  # Configuraci\u00f3n de IA (proveedor, modelo, api key, etc.)
  ai_config <- shiny::reactiveVal(config_load())

  # Historial de sesi\u00f3n
  session_history <- shiny::reactiveVal(list())

  # -- Inicializaci\u00f3n ----------------------------------------------------
  # Llamada directa (sin observe) - solo se ejecuta una vez al iniciar
  history_log(session_history, type = "session_start",
              detail = list(timestamp = as.character(Sys.time())))

  # -- M\u00f3dulos -----------------------------------------------------------
  mod_dataset_server(
    id             = "dataset",
    active_dataset = active_dataset,
    active_name    = active_dataset_name,
    history        = session_history
  )

  mod_explorer_server(
    id             = "explorer",
    active_dataset = active_dataset,
    active_name    = active_dataset_name
  )

  mod_builder_server(
    id              = "builder",
    active_dataset  = active_dataset,
    active_name     = active_dataset_name,
    operation_stack = operation_stack,
    generated_code  = generated_code,
    history         = session_history
  )

  mod_code_server(
    id             = "code",
    generated_code = generated_code,
    active_dataset = active_dataset,
    active_name    = active_dataset_name
  )

  mod_ai_server(
    id              = "ai",
    active_dataset  = active_dataset,
    active_name     = active_dataset_name,
    operation_stack = operation_stack,
    generated_code  = generated_code,
    ai_config       = ai_config,
    history         = session_history,
    parent_session  = session
  )

  mod_config_server(
    id        = "config",
    ai_config = ai_config
  )

  # -- Historial modal ---------------------------------------------------
  shiny::observeEvent(input$btn_history, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Historial de operaciones",
        mod_history_ui_inline(session_history()),
        easyClose = TRUE,
        footer = shiny::modalButton("Cerrar")
      )
    )
  })
}
