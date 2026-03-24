#' Módulo: Panel de Código R Generado
#'
#' Muestra el código R producido por el Constructor en tiempo real.
#' Permite copiar, insertar en script activo, ejecutar, y guardar como .R.

# ── UI ────────────────────────────────────────────────────────────────────
mod_code_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header(
      shiny::icon("code"), " Código R generado",
      bslib::tooltip(shiny::icon("circle-info"),
        "Código generado por tus acciones visuales. Podés copiarlo, ejecutarlo o guardarlo.")
    ),
    shiny::div(
      class = "d-flex gap-2 mb-3 flex-wrap",
      shiny::actionButton(ns("btn_copy"),   "Copiar",
        icon = shiny::icon("copy"),        class = "btn-sm btn-outline-secondary"),
      shiny::actionButton(ns("btn_insert"), "Insertar en script",
        icon = shiny::icon("file-import"), class = "btn-sm btn-outline-primary"),
      shiny::actionButton(ns("btn_run"),    "Ejecutar",
        icon = shiny::icon("play"),        class = "btn-sm btn-success"),
      shiny::downloadButton(ns("btn_save"), "Guardar .R",
        icon = shiny::icon("floppy-disk"), class = "btn-sm btn-outline-secondary")
    ),
    shiny::tags$pre(
      id    = ns("code_display"),
      class = "bg-light border rounded p-3",
      style = "min-height:280px; font-size:13px; overflow:auto;",
      shiny::textOutput(ns("code_text"), inline = FALSE)
    ),
    bslib::card_footer(
      shiny::uiOutput(ns("execution_result"))
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────
mod_code_server <- function(id, generated_code, active_dataset, active_name) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Mostrar código ────────────────────────────────────────────────────
    output$code_text <- shiny::renderText({
      code <- generated_code()
      if (is.null(code) || nchar(trimws(code)) == 0)
        "# El código R aparecerá aquí cuando construyas operaciones en el Constructor."
      else code
    })

    # ── Copiar al portapapeles ────────────────────────────────────────────
    shiny::observeEvent(input$btn_copy, {
      shiny::req(generated_code())
      session$sendCustomMessage("rvisual_copy_to_clipboard", generated_code())
      shiny::showNotification("Código copiado al portapapeles.", type = "message", duration = 2)
    })

    # ── Insertar en script activo de RStudio ──────────────────────────────
    shiny::observeEvent(input$btn_insert, {
      shiny::req(generated_code())
      tryCatch({
        rstudio_insert_code(generated_code())
        shiny::showNotification("Código insertado en el script activo.", type = "message")
      }, error = function(e) {
        shiny::showNotification(
          paste("No se pudo insertar (¿hay un script abierto?):", e$message),
          type = "warning")
      })
    })

    # ── Ejecutar con confirmación ─────────────────────────────────────────
    execution_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$btn_run, {
      shiny::req(generated_code())
      shiny::showModal(shiny::modalDialog(
        title = "Confirmar ejecución",
        shiny::p("¿Ejecutar el siguiente código en tu entorno R?"),
        shiny::tags$pre(class = "bg-light p-2", style = "font-size:12px;",
                        generated_code()),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("confirm_run"), "Ejecutar", class = "btn-success")
        )
      ))
    })

    shiny::observeEvent(input$confirm_run, {
      shiny::removeModal()
      result <- tryCatch({
        output_lines <- utils::capture.output({
          val <- eval(parse(text = generated_code()), envir = .GlobalEnv)
          if (!is.null(val)) print(val)
        })
        list(success = TRUE, message = "Ejecutado correctamente.",
             output = if (length(output_lines) > 0) output_lines else NULL)
      }, error = function(e) {
        list(success = FALSE, message = paste("Error:", e$message), output = NULL)
      })
      execution_result(result)
    })

    output$execution_result <- shiny::renderUI({
      res <- execution_result()
      if (is.null(res)) return(NULL)
      status_box <- if (res$success)
        shiny::div(class = "alert alert-success mb-2",
                   shiny::icon("check-circle"), " ", res$message)
      else
        shiny::div(class = "alert alert-danger mb-2",
                   shiny::icon("triangle-exclamation"), " ", res$message)
      output_box <- if (!is.null(res$output))
        shiny::div(
          shiny::tags$strong("Resultado:", style = "font-size:13px;"),
          shiny::tags$pre(
            class = "bg-dark text-light rounded p-3 mt-1",
            style = "font-size:12px; max-height:300px; overflow-y:auto;",
            paste(res$output, collapse = "\n"))
        )
      shiny::tagList(status_box, output_box)
    })

    # ── Guardar como .R ───────────────────────────────────────────────────
    output$btn_save <- shiny::downloadHandler(
      filename = function() {
        nm   <- active_name()
        base <- if (!is.null(nm) && nchar(nm) > 0) nm else "rvisual"
        paste0(base, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".R")
      },
      content = function(file) {
        code <- generated_code()
        if (is.null(code) || nchar(trimws(code)) == 0)
          code <- "# Sin operaciones generadas aún.\n"
        writeLines(code, file)
      },
      contentType = "text/plain"
    )

  })
}
