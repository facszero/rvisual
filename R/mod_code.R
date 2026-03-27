#' M\u00f3dulo: Panel de C\u00f3digo R Generado
#'
#' Muestra el c\u00f3digo R producido por el Constructor en tiempo real.
#' Permite copiar, insertar en script, ejecutar, guardar como .R
#' y exportar el resultado a CSV o Excel.

# ── UI ────────────────────────────────────────────────────────────────────
mod_code_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header(
      shiny::icon("code"), " C\u00f3digo R generado",
      bslib::tooltip(shiny::icon("circle-info"),
        "C\u00f3digo generado por tus acciones visuales. Pod\u00e9s copiarlo, ejecutarlo, guardarlo o exportar el resultado.")
    ),
    shiny::div(
      class = "d-flex gap-2 mb-3 flex-wrap",
      # Fila 1: acciones sobre el c\u00f3digo
      shiny::actionButton(ns("btn_copy"),   "Copiar",
        icon = shiny::icon("copy"),        class = "btn-sm btn-outline-secondary"),
      shiny::actionButton(ns("btn_insert"), "Insertar en script",
        icon = shiny::icon("file-import"), class = "btn-sm btn-outline-primary"),
      shiny::actionButton(ns("btn_run"),    "Ejecutar",
        icon = shiny::icon("play"),        class = "btn-sm btn-success"),
      shiny::downloadButton(ns("btn_save"), "Guardar .R",
        icon = shiny::icon("floppy-disk"), class = "btn-sm btn-outline-secondary"),
      # Separador visual
      shiny::tags$span(style = "border-left:1px solid #dee2e6; margin:0 4px;"),
      # Fila 2: exportar resultado
      shiny::downloadButton(ns("btn_export_csv"),  "Resultado \u2192 CSV",
        icon = shiny::icon("file-csv"),   class = "btn-sm btn-outline-success"),
      shiny::downloadButton(ns("btn_export_xlsx"), "Resultado \u2192 Excel",
        icon = shiny::icon("file-excel"), class = "btn-sm btn-outline-success")
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

    # Almacena el \u00faltimo resultado de la ejecuci\u00f3n (data.frame o valor)
    last_result     <- shiny::reactiveVal(NULL)
    execution_result <- shiny::reactiveVal(NULL)

    # ── Mostrar c\u00f3digo ─────────────────────────────────────────────────────
    output$code_text <- shiny::renderText({
      code <- generated_code()
      if (is.null(code) || nchar(trimws(code)) == 0)
        "# El c\u00f3digo R aparecer\u00e1 aqu\u00ed cuando construyas operaciones en el Constructor."
      else code
    })

    # ── Copiar al portapapeles ────────────────────────────────────────────
    shiny::observeEvent(input$btn_copy, {
      shiny::req(generated_code())
      session$sendCustomMessage("rvisual_copy_to_clipboard", generated_code())
      shiny::showNotification("C\u00f3digo copiado al portapapeles.", type = "message", duration = 2)
    })

    # ── Insertar en script activo de RStudio ──────────────────────────────
    shiny::observeEvent(input$btn_insert, {
      shiny::req(generated_code())
      tryCatch({
        rstudio_insert_code(generated_code())
        shiny::showNotification("C\u00f3digo insertado en el script activo.", type = "message")
      }, error = function(e) {
        shiny::showNotification(
          paste("No se pudo insertar:", e$message), type = "warning")
      })
    })

    # ── Ejecutar con confirmaci\u00f3n ─────────────────────────────────────────
    shiny::observeEvent(input$btn_run, {
      shiny::req(generated_code())
      shiny::showModal(shiny::modalDialog(
        title = "Confirmar ejecuci\u00f3n",
        shiny::p("\u00bfEjecutar el siguiente c\u00f3digo en tu entorno R?"),
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
        val          <- NULL
        output_lines <- utils::capture.output({
          val <- eval(parse(text = generated_code()), envir = .GlobalEnv)
          if (!is.null(val)) print(val)
        })
        # Guardar el valor para exportaci\u00f3n
        last_result(val)
        list(success = TRUE,
             message = "Ejecutado correctamente.",
             output  = if (length(output_lines) > 0) output_lines else NULL,
             is_df   = is.data.frame(val))
      }, error = function(e) {
        last_result(NULL)
        list(success = FALSE, message = paste("Error:", e$message),
             output = NULL, is_df = FALSE)
      })
      execution_result(result)
    })

    output$execution_result <- shiny::renderUI({
      res <- execution_result()
      if (is.null(res)) return(NULL)

      status_box <- if (res$success)
        shiny::div(class = "alert alert-success mb-2",
                   shiny::icon("check-circle"), " ", res$message,
                   if (isTRUE(res$is_df))
                     shiny::tags$small(class = "ms-2 text-muted",
                       "(resultado disponible para exportar)")
                  )
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

    # ── Guardar c\u00f3digo como .R ──────────────────────────────────────────────
    output$btn_save <- shiny::downloadHandler(
      filename = function() {
        nm   <- active_name()
        base <- if (!is.null(nm) && nchar(nm) > 0) nm else "rvisual"
        paste0(base, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".R")
      },
      content = function(file) {
        code <- generated_code()
        if (is.null(code) || nchar(trimws(code)) == 0)
          code <- "# Sin operaciones generadas a\u00fan.\n"
        writeLines(code, file)
      },
      contentType = "text/plain"
    )

    # ── Exportar resultado como CSV ───────────────────────────────────────
    output$btn_export_csv <- shiny::downloadHandler(
      filename = function() {
        nm   <- active_name()
        base <- if (!is.null(nm) && nchar(nm) > 0)
          paste0(nm, "_resultado") else "rvisual_resultado"
        paste0(base, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        df <- last_result()
        if (is.null(df)) {
          # Si no hay resultado previo, ejecutar el c\u00f3digo ahora
          df <- tryCatch(
            eval(parse(text = generated_code()), envir = .GlobalEnv),
            error = function(e) NULL
          )
        }
        if (is.null(df) || !is.data.frame(df)) {
          # Exportar mensaje de error como CSV m\u00ednimo
          write.csv(data.frame(error = "El c\u00f3digo no produjo un data.frame exportable."),
                    file, row.names = FALSE)
          shiny::showNotification(
            "El resultado no es un data.frame. Execute primero para verificar.",
            type = "warning", duration = 5)
        } else {
          write.csv(df, file, row.names = FALSE)
          shiny::showNotification(
            paste0("Exportado: ", nrow(df), " filas \u00d7 ", ncol(df), " columnas"),
            type = "message", duration = 3)
        }
      },
      contentType = "text/csv"
    )

    # ── Exportar resultado como Excel ─────────────────────────────────────
    output$btn_export_xlsx <- shiny::downloadHandler(
      filename = function() {
        nm   <- active_name()
        base <- if (!is.null(nm) && nchar(nm) > 0)
          paste0(nm, "_resultado") else "rvisual_resultado"
        paste0(base, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
      },
      content = function(file) {
        df <- last_result()
        if (is.null(df)) {
          df <- tryCatch(
            eval(parse(text = generated_code()), envir = .GlobalEnv),
            error = function(e) NULL
          )
        }
        if (is.null(df) || !is.data.frame(df)) {
          write.csv(data.frame(error = "El c\u00f3digo no produjo un data.frame exportable."),
                    file, row.names = FALSE)
          shiny::showNotification(
            "El resultado no es un data.frame. Ejecut\u00e1 primero para verificar.",
            type = "warning", duration = 5)
        } else if (!requireNamespace("writexl", quietly = TRUE)) {
          write.csv(df, file, row.names = FALSE)
          shiny::showNotification(
            "writexl no instalado \u2014 se export\u00f3 como CSV. Instal\u00e1 con: install.packages('writexl')",
            type = "warning", duration = 8)
        } else {
          writexl::write_xlsx(df, file)
          shiny::showNotification(
            paste0("Exportado: ", nrow(df), " filas \u00d7 ", ncol(df), " columnas"),
            type = "message", duration = 3)
        }
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

  })
}
