#' Módulo: Asistente IA Contextual
#'
#' Panel de chat con agente IA multi-proveedor.
#' El agente conoce:
#'   - El nombre y esquema del dataset activo
#'   - Los tipos de columnas
#'   - El stack de operaciones actual
#'   - Opcionalmente, una muestra limitada de datos
#'
#' Modalidad segura: el agente propone código y el usuario decide si ejecutarlo.

# ── UI ────────────────────────────────────────────────────────────────────
mod_ai_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(8, 4),

    # Chat principal
    bslib::card(
      bslib::card_header(shiny::icon("robot"), " Asistente RVisual IA"),
      shiny::div(
        id    = ns("chat_messages"),
        style = "height:420px; overflow-y:auto; padding:12px;",
        shiny::uiOutput(ns("messages_ui"))
      ),
      bslib::card_footer(
        shiny::div(
          class = "d-flex gap-2",
          shiny::textAreaInput(
            ns("user_prompt"), NULL,
            placeholder = "Ej: filtrá año == 2025, agrupá por región y calculá promedio de ingreso",
            rows = 2
          ),
          shiny::div(
            class = "d-flex flex-column gap-1",
            shiny::actionButton(ns("btn_send"), "Enviar",
                                icon = shiny::icon("paper-plane"), class = "btn-primary"),
            shiny::actionButton(ns("btn_clear_chat"), "Limpiar",
                                icon = shiny::icon("broom"), class = "btn-sm btn-outline-secondary")
          )
        )
      )
    ),

    # Panel derecho: contexto + controles
    bslib::card(
      bslib::card_header("Contexto del agente"),
      shiny::uiOutput(ns("context_summary")),
      shiny::hr(),
      shiny::checkboxInput(ns("include_sample"), "Incluir muestra de datos (5 filas)", value = FALSE),
      bslib::tooltip(
        shiny::icon("triangle-exclamation", class = "text-warning"),
        "Activar solo con datos no sensibles. Las filas se enviarán al proveedor IA externo."
      ),
      shiny::hr(),
      shiny::uiOutput(ns("provider_badge"))
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────
mod_ai_server <- function(id, active_dataset, active_name, operation_stack,
                           generated_code, ai_config, history) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Historial de mensajes del chat
    chat_history <- shiny::reactiveVal(list())

    # Mostrar contexto disponible para el agente
    output$context_summary <- shiny::renderUI({
      df <- active_dataset()
      if (is.null(df)) {
        return(shiny::p("Sin dataset activo.", class = "text-muted small"))
      }
      shiny::tagList(
        shiny::tags$strong(active_name()),
        shiny::tags$br(),
        shiny::tags$small(
          glue::glue("{nrow(df)} filas × {ncol(df)} columnas"),
          class = "text-muted"
        ),
        shiny::tags$br(),
        shiny::tags$small(
          paste(names(df)[1:min(5, ncol(df))], collapse = ", "),
          if (ncol(df) > 5) "...",
          class = "text-muted"
        )
      )
    })

    # Badge del proveedor activo
    output$provider_badge <- shiny::renderUI({
      cfg <- ai_config()
      if (is.null(cfg) || is.null(cfg$provider)) {
        return(shiny::div(class = "alert alert-warning small p-2",
                          "Configurá un proveedor IA en la pestaña Configuración."))
      }
      shiny::div(class = "badge bg-secondary",
                 glue::glue("{cfg$provider} / {cfg$model}"))
    })

    # Renderizar mensajes del chat
    output$messages_ui <- shiny::renderUI({
      msgs <- chat_history()
      if (length(msgs) == 0) {
        return(shiny::p(
          "Describí en lenguaje natural lo que querés hacer con tus datos.",
          class = "text-muted"
        ))
      }
      shiny::tagList(lapply(msgs, render_chat_message_ui))
    })

    # Enviar prompt al agente
    shiny::observeEvent(input$btn_send, {
      shiny::req(input$user_prompt, nchar(trimws(input$user_prompt)) > 0)
      shiny::req(active_dataset())

      cfg <- ai_config()
      if (is.null(cfg) || is.null(cfg$provider)) {
        shiny::showNotification("Configurá un proveedor IA primero.", type = "warning")
        return()
      }

      prompt_text <- trimws(input$user_prompt)

      # Agregar mensaje del usuario al chat
      append_message(chat_history, role = "user", content = prompt_text)
      shiny::updateTextAreaInput(session, "user_prompt", value = "")

      # Construir contexto para el agente
      ctx <- build_ai_context(
        df           = active_dataset(),
        df_name      = active_name(),
        ops          = operation_stack(),
        include_rows = input$include_sample
      )

      # Llamar al proveedor IA con manejo de loading
      append_message(chat_history, role = "assistant_loading", content = "...")

      shiny::withProgress(message = "Consultando IA...", value = 0.5, {
        response <- tryCatch(
          ai_send(cfg, system_prompt = build_system_prompt(ctx), user_prompt = prompt_text),
          error = function(e) {
            msg <- e$message
            # Detectar error de red del viewer de RStudio
            if (grepl("HTTP request|curl|conexi|connect|network|timeout", msg, ignore.case = TRUE)) {
              msg <- paste0(
                "No se pudo conectar con el proveedor IA.\n\n",
                "El panel embebido de RStudio bloquea conexiones HTTP salientes.\n",
                "Solución: reiniciá con launch_rvisual(browser = TRUE)"
              )
            }
            list(error = msg)
          }
        )
      })

      # Reemplazar mensaje de loading con respuesta real
      msgs       <- chat_history()
      last_idx   <- length(msgs)
      msgs[[last_idx]] <- NULL  # Remover loading
      chat_history(msgs)

      if (!is.null(response$error)) {
        append_message(chat_history, role = "error",
                       content = paste("Error al contactar IA:", response$error))
      } else {
        append_message(chat_history, role = "assistant",
                       content = response$text,
                       code    = response$code)
      }

      history_log(history, "ai_query", list(prompt = prompt_text))
    })

    # Limpiar chat
    shiny::observeEvent(input$btn_clear_chat, {
      chat_history(list())
    })

    # Ejecutar código propuesto por IA (acción del usuario)
    shiny::observe({
      msgs <- chat_history()
      lapply(seq_along(msgs), function(i) {
        msg <- msgs[[i]]
        if (!is.null(msg$code) && !is.null(msg$code_id)) {
          btn_id <- paste0("run_ai_code_", msg$code_id)
          shiny::observeEvent(input[[btn_id]], {
            tryCatch({
              eval(parse(text = msg$code), envir = .GlobalEnv)
              shiny::showNotification("Código ejecutado correctamente.", type = "message")
              history_log(history, "ai_code_executed", list(code = msg$code))
            }, error = function(e) {
              shiny::showNotification(paste("Error:", e$message), type = "error")
            })
          }, ignoreInit = TRUE, once = TRUE)
        }
      })
    })

  })
}

# ── Helpers ───────────────────────────────────────────────────────────────

append_message <- function(chat_history, role, content, code = NULL) {
  msg <- list(
    role      = role,
    content   = content,
    code      = code,
    code_id   = if (!is.null(code)) paste0("c", as.integer(Sys.time())),
    timestamp = format(Sys.time(), "%H:%M")
  )
  chat_history(c(chat_history(), list(msg)))
}

render_chat_message_ui <- function(msg) {
  is_user <- msg$role == "user"
  bg      <- if (is_user) "bg-primary text-white" else "bg-light border"
  align   <- if (is_user) "text-end" else "text-start"

  shiny::div(
    class = paste("mb-3", align),
    shiny::div(
      class = paste("d-inline-block rounded p-2 px-3", bg),
      style = "max-width:90%; font-size:14px;",
      shiny::p(msg$content, class = "mb-1"),
      # Si hay código, mostrar bloque con botón de ejecutar
      if (!is.null(msg$code)) {
        shiny::tagList(
          shiny::tags$pre(class = "bg-white border rounded p-2 mt-2",
                          style = "font-size:12px;", msg$code),
          shiny::actionButton(
            paste0("run_ai_code_", msg$code_id),
            "Ejecutar este código",
            icon  = shiny::icon("play"),
            class = "btn-sm btn-success mt-1"
          )
        )
      }
    ),
    shiny::tags$small(msg$timestamp, class = "text-muted d-block mt-1")
  )
}
