#' M\u00f3dulo: Asistente IA Contextual
#'
#' Panel de chat con agente IA multi-proveedor.
#' El agente conoce:
#'   - El nombre y esquema del dataset activo
#'   - Los tipos de columnas
#'   - El stack de operaciones actual
#'   - Opcionalmente, una muestra limitada de datos
#'
#' Modalidad segura: el agente propone c\u00f3digo y el usuario decide si ejecutarlo.

# -- UI --------------------------------------------------------------------
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
            placeholder = "Ej: filtr\u00e1 a\u00f1o == 2025, agrup\u00e1 por regi\u00f3n y calcul\u00e1 promedio de ingreso",
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
        "Activar solo con datos no sensibles. Las filas se enviar\u00e1n al proveedor IA externo."
      ),
      shiny::hr(),
      shiny::uiOutput(ns("provider_badge")),
      shiny::hr(),
      shiny::actionButton(ns("btn_test_conn"), "Probar conexi\u00f3n",
                          icon = shiny::icon("plug"), class = "btn-sm btn-outline-secondary w-100"),
      shiny::uiOutput(ns("conn_status"))
    )
  )
}

# -- Server ----------------------------------------------------------------
mod_ai_server <- function(id, active_dataset, active_name, operation_stack,
                           generated_code, ai_config, history, parent_session) {
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
          glue::glue("{nrow(df)} filas \u00d7 {ncol(df)} columnas"),
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
                          "Configur\u00e1 un proveedor IA en la pesta\u00f1a Configuraci\u00f3n."))
      }
      shiny::div(class = "badge bg-secondary",
                 glue::glue("{cfg$provider} / {cfg$model}"))
    })

    # Renderizar mensajes del chat
    output$messages_ui <- shiny::renderUI({
      msgs <- chat_history()
      if (length(msgs) == 0) {
        return(shiny::p(
          "Describ\u00ed en lenguaje natural lo que quer\u00e9s hacer con tus datos.",
          class = "text-muted"
        ))
      }
      shiny::tagList(lapply(msgs, render_chat_message_ui, ns = ns))
    })

    # Enviar prompt al agente
    shiny::observeEvent(input$btn_send, {
      shiny::req(input$user_prompt, nchar(trimws(input$user_prompt)) > 0)
      shiny::req(active_dataset())

      cfg <- ai_config()
      if (is.null(cfg) || is.null(cfg$provider)) {
        shiny::showNotification("Configur\u00e1 un proveedor IA primero.", type = "warning")
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
          error = function(e) list(error = e$message)
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

    # Probar conexi\u00f3n
    shiny::observeEvent(input$btn_test_conn, {
      cfg <- ai_config()
      if (is.null(cfg) || is.null(cfg$provider)) {
        output$conn_status <- shiny::renderUI(
          shiny::div(class = "alert alert-warning small p-2 mt-2",
                     "Configur\u00e1 un proveedor en la pesta\u00f1a Configuraci\u00f3n."))
        return()
      }
      output$conn_status <- shiny::renderUI(
        shiny::div(class = "alert alert-info small p-2 mt-2", "Probando..."))
      result <- tryCatch(
        ai_test_connection(cfg),
        error = function(e) list(success = FALSE, message = e$message)
      )
      output$conn_status <- shiny::renderUI({
        if (result$success) {
          shiny::div(class = "alert alert-success small p-2 mt-2",
                     shiny::icon("check"), " Conexi\u00f3n OK")
        } else {
          shiny::div(class = "alert alert-danger small p-2 mt-2",
                     shiny::icon("triangle-exclamation"), " ",
                     result$message)
        }
      })
    })

    # -- Enviar c\u00f3digo al panel C\u00f3digo R --------------------------------------
    shiny::observe({
      msgs <- chat_history()
      lapply(seq_along(msgs), function(i) {
        msg <- msgs[[i]]
        if (!is.null(msg$code) && !is.null(msg$code_id)) {
          btn_id <- paste0("send_to_code_", msg$code_id)
          shiny::observeEvent(input[[btn_id]], {
            generated_code(msg$code)
            # Navegar a la pesta\u00f1a C\u00f3digo R
            bslib::nav_select("main_nav", "panel_code", session = parent_session)
            shiny::showNotification(
              "\u2192 C\u00f3digo enviado a la pesta\u00f1a C\u00f3digo R",
              type = "message", duration = 3
            )
            history_log(history, "ai_code_sent_to_panel", list(code_id = msg$code_id))
          }, ignoreInit = TRUE, once = TRUE)
        }
      })
    })

    # -- Ejecutar c\u00f3digo directo desde el chat --------------------------------
    shiny::observe({
      msgs <- chat_history()
      lapply(seq_along(msgs), function(i) {
        msg <- msgs[[i]]
        if (!is.null(msg$code) && !is.null(msg$code_id)) {
          btn_id <- paste0("run_ai_code_", msg$code_id)
          shiny::observeEvent(input[[btn_id]], {
            output_lines <- tryCatch({
              utils::capture.output({
                val <- eval(parse(text = msg$code), envir = .GlobalEnv)
                if (!is.null(val)) print(val)
              })
            }, error = function(e) {
              paste("Error:", e$message)
            })
            # Mostrar resultado como notificaci\u00f3n + append al chat
            if (length(output_lines) > 0 && !grepl("^Error:", output_lines[1])) {
              shiny::showNotification("Ejecutado correctamente.", type = "message")
              # Agregar resultado al chat como mensaje especial
              append_message(chat_history,
                             role    = "result",
                             content = paste(output_lines, collapse = "\n"))
            } else {
              shiny::showNotification(paste(output_lines, collapse = " "), type = "error")
            }
            history_log(history, "ai_code_executed", list(code = msg$code))
          }, ignoreInit = TRUE, once = TRUE)
        }
      })
    })

  })
}

# -- Helpers ---------------------------------------------------------------

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

render_chat_message_ui <- function(msg, ns = identity) {
  is_user   <- msg$role == "user"
  is_result <- msg$role == "result"

  if (is_result) {
    return(shiny::div(
      class = "mb-3",
      shiny::div(
        class = "border rounded p-2",
        style = "background:#1e1e1e; font-family:monospace; font-size:12px;
                 color:#d4d4d4; white-space:pre-wrap; max-height:250px; overflow-y:auto;",
        shiny::tags$span(shiny::icon("terminal"),
                         style = "color:#4ec9b0; margin-right:6px;"),
        msg$content
      ),
      shiny::tags$small(msg$timestamp, class = "text-muted d-block mt-1")
    ))
  }

  bg    <- if (is_user) "bg-primary text-white" else "bg-light border"
  align <- if (is_user) "text-end" else "text-start"

  shiny::div(
    class = paste("mb-3", align),
    shiny::div(
      class = paste("d-inline-block rounded p-2 px-3", bg),
      style = "max-width:90%; font-size:14px;",
      shiny::p(msg$content, class = "mb-1"),
      if (!is.null(msg$code)) {
        shiny::tagList(
          shiny::tags$pre(class = "bg-white border rounded p-2 mt-2",
                          style = "font-size:12px; white-space:pre-wrap;", msg$code),
          shiny::div(class = "d-flex gap-2 mt-1",
            shiny::actionButton(
              ns(paste0("send_to_code_", msg$code_id)),
              "\u2192 Enviar a C\u00f3digo R",
              icon  = shiny::icon("code"),
              class = "btn-sm btn-primary"
            ),
            shiny::actionButton(
              ns(paste0("run_ai_code_", msg$code_id)),
              "Ejecutar directo",
              icon  = shiny::icon("play"),
              class = "btn-sm btn-outline-success"
            )
          )
        )
      }
    ),
    shiny::tags$small(msg$timestamp, class = "text-muted d-block mt-1")
  )
}
