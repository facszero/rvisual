#' Capa de Integración con IA
#'
#' Interfaz común para múltiples proveedores LLM.
#' Cada proveedor implementa la misma firma, permitiendo
#' intercambiarlos sin tocar los módulos de UI.

# ── Interfaz común ────────────────────────────────────────────────────────

#' Enviar un prompt al proveedor IA configurado
#'
#' @param cfg         Lista de configuración: provider, model, api_key
#' @param system_prompt String con el prompt de sistema (contexto del dataset)
#' @param user_prompt   String con el mensaje del usuario
#' @return Lista con: text (respuesta completa), code (código R extraído o NULL)
#' @export
ai_send <- function(cfg, system_prompt, user_prompt) {
  stopifnot(!is.null(cfg$provider), !is.null(cfg$api_key))

  response_text <- switch(cfg$provider,
    "openai"    = provider_openai(cfg, system_prompt, user_prompt),
    "anthropic" = provider_anthropic(cfg, system_prompt, user_prompt),
    "gemini"    = provider_gemini(cfg, system_prompt, user_prompt),
    stop(glue::glue("Proveedor no soportado: {cfg$provider}"))
  )

  # Extraer bloque de código R si existe
  code_block <- extract_r_code_block(response_text)

  # Validar que el código no referencie columnas inexistentes (si tenemos contexto)
  # TODO: conectar con validate_code_columns() en Fase 5

  list(text = response_text, code = code_block)
}

#' Probar conexión con el proveedor
#' @export
ai_test_connection <- function(cfg) {
  tryCatch({
    res <- ai_send(cfg,
                   system_prompt = "Eres un asistente de prueba.",
                   user_prompt   = "Respondé solo: OK")
    list(success = TRUE, message = res$text)
  }, error = function(e) {
    list(success = FALSE, message = e$message)
  })
}

# ── Proveedores ───────────────────────────────────────────────────────────

provider_anthropic <- function(cfg, system_prompt, user_prompt) {
  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key"         = cfg$api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    httr2::req_body_json(list(
      model      = cfg$model %||% "claude-haiku-4-5-20251001",
      max_tokens = 2048,
      system     = system_prompt,
      messages   = list(list(role = "user", content = user_prompt))
    )) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp)
  if (!is.null(parsed$error)) stop(parsed$error$message)
  parsed$content[[1]]$text
}

provider_openai <- function(cfg, system_prompt, user_prompt) {
  resp <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", cfg$api_key),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(
      model    = cfg$model %||% "gpt-4o-mini",
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user",   content = user_prompt)
      )
    )) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp)
  if (!is.null(parsed$error)) stop(parsed$error$message)
  parsed$choices[[1]]$message$content
}

provider_gemini <- function(cfg, system_prompt, user_prompt) {
  model_id <- cfg$model %||% "gemini-1.5-flash"
  url      <- glue::glue(
    "https://generativelanguage.googleapis.com/v1beta/models/{model_id}:generateContent?key={cfg$api_key}"
  )
  resp <- httr2::request(url) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(list(
      contents = list(list(
        parts = list(list(text = paste0(system_prompt, "\n\n", user_prompt)))
      ))
    )) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp)
  if (!is.null(parsed$error)) stop(parsed$error$message)
  parsed$candidates[[1]]$content$parts[[1]]$text
}

# ── Construcción de contexto ──────────────────────────────────────────────

#' Construir prompt de sistema con contexto del dataset activo
#' @export
build_system_prompt <- function(ctx) {
  glue::glue(
    "Sos un asistente de análisis de datos especializado en R y tidyverse.\n",
    "Tu rol es ayudar a usuarios que vienen de SPSS a trabajar en R.\n\n",
    "DATASET ACTIVO: {ctx$df_name}\n",
    "Filas: {ctx$nrow} | Columnas: {ctx$ncol}\n\n",
    "VARIABLES DISPONIBLES:\n{ctx$schema_text}\n\n",
    if (!is.null(ctx$sample_text)) paste0("MUESTRA DE DATOS (5 filas):\n", ctx$sample_text, "\n\n") else "",
    if (length(ctx$ops) > 0) paste0("OPERACIONES YA APLICADAS:\n", ctx$ops_text, "\n\n") else "",
    "REGLAS:\n",
    "1. Generá SOLO código R usando dplyr/tidyr/pipe nativo (|>).\n",
    "2. No inventes columnas que no estén en la lista de variables.\n",
    "3. Si el pedido es ambiguo, preguntá antes de asumir.\n",
    "4. Explicá brevemente qué hace el código.\n",
    "5. Encerrá el código R en bloque ```r ... ```.\n",
    "6. Si el usuario menciona sintaxis SPSS, traducila al equivalente en R."
  )
}

#' Construir contexto del dataset para enviar al agente
#' @export
build_ai_context <- function(df, df_name, ops, include_rows = FALSE) {
  meta <- get_metadata(df, df_name)

  schema_lines <- apply(meta$columns, 1, function(row) {
    glue::glue("  - {row['name']} ({row['visual']})")
  })

  sample_text <- if (include_rows && nrow(df) > 0) {
    utils::capture.output(print(head(df, 5)))
  } else {
    NULL
  }

  ops_text <- if (length(ops) > 0) {
    paste(vapply(ops, function(o) paste0("  - ", o$label), character(1)), collapse = "\n")
  } else {
    NULL
  }

  list(
    df_name     = df_name,
    nrow        = meta$nrow,
    ncol        = meta$ncol,
    schema_text = paste(schema_lines, collapse = "\n"),
    sample_text = if (!is.null(sample_text)) paste(sample_text, collapse = "\n") else NULL,
    ops         = ops,
    ops_text    = ops_text
  )
}

# ── Extracción de código de la respuesta ──────────────────────────────────

extract_r_code_block <- function(text) {
  # Buscar bloque ```r ... ``` o ``` ... ```
  pattern <- "```(?:r|R)?\\s*\n?(.*?)```"
  matches <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  if (length(matches) == 0 || nchar(matches) == 0) return(NULL)

  # Limpiar delimitadores
  code <- gsub("```(?:r|R)?\\s*\n?|```", "", matches, perl = TRUE)
  trimws(code)
}

# ── Operador null-coalesce ────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && nchar(a) > 0) a else b
