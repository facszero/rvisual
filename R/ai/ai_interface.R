#' Capa de Integración con IA
#'
#' Usa curl directamente para mayor compatibilidad con proxies corporativos.

# ── Interfaz común ────────────────────────────────────────────────────────

#' @export
ai_send <- function(cfg, system_prompt, user_prompt) {
  stopifnot(!is.null(cfg$provider), !is.null(cfg$api_key))

  response_text <- switch(cfg$provider,
    "openai"    = provider_openai(cfg, system_prompt, user_prompt),
    "anthropic" = provider_anthropic(cfg, system_prompt, user_prompt),
    "gemini"    = provider_gemini(cfg, system_prompt, user_prompt),
    stop(glue::glue("Proveedor no soportado: {cfg$provider}"))
  )

  code_block <- extract_r_code_block(response_text)
  list(text = response_text, code = code_block)
}

#' @export
ai_test_connection <- function(cfg) {
  # Primero verificar conectividad básica a internet
  internet_ok <- tryCatch({
    con <- url("https://www.google.com", open = "r", timeout = 5)
    close(con)
    TRUE
  }, error = function(e) FALSE)

  if (!internet_ok) {
    return(list(success = FALSE,
                message = "Sin acceso a internet desde R. Verificá proxy o firewall."))
  }

  tryCatch({
    res <- ai_send(cfg,
                   system_prompt = "Eres un asistente de prueba.",
                   user_prompt   = "Respondé solo: OK")
    list(success = TRUE, message = res$text)
  }, error = function(e) {
    list(success = FALSE, message = e$message)
  })
}

# ── Helper HTTP unificado ─────────────────────────────────────────────────
# Usa jsonlite + curl vía utils::download.file como fallback si httr2 falla

http_post_json <- function(url, headers, body_list) {
  body_json <- jsonlite::toJSON(body_list, auto_unbox = TRUE)

  # Intentar con httr2 primero
  result <- tryCatch({
    req <- httr2::request(url)
    for (nm in names(headers)) {
      req <- httr2::req_headers(req, !!nm := headers[[nm]])
    }
    req <- req |>
      httr2::req_body_raw(body_json, type = "application/json") |>
      httr2::req_timeout(45) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    httr2::resp_body_json(req)
  }, error = function(e1) {
    # Fallback: usar curl del sistema si está disponible
    if (nzchar(Sys.which("curl"))) {
      header_args <- paste(
        sapply(names(headers), function(h) paste0('-H "', h, ': ', headers[[h]], '"')),
        collapse = " "
      )
      tmp_body <- tempfile(fileext = ".json")
      tmp_out  <- tempfile(fileext = ".json")
      writeLines(body_json, tmp_body)
      cmd <- sprintf('curl -s -X POST %s -d @"%s" "%s" -o "%s"',
                     header_args, tmp_body, url, tmp_out)
      system(cmd, wait = TRUE)
      if (file.exists(tmp_out) && file.size(tmp_out) > 0) {
        jsonlite::fromJSON(tmp_out, simplifyVector = FALSE)
      } else {
        stop(e1$message)
      }
    } else {
      stop(e1$message)
    }
  })
  result
}

# ── Proveedores ───────────────────────────────────────────────────────────

provider_anthropic <- function(cfg, system_prompt, user_prompt) {
  parsed <- http_post_json(
    url     = "https://api.anthropic.com/v1/messages",
    headers = list(
      "x-api-key"         = cfg$api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ),
    body_list = list(
      model      = cfg$model %||% "claude-haiku-4-5-20251001",
      max_tokens = 2048,
      system     = system_prompt,
      messages   = list(list(role = "user", content = user_prompt))
    )
  )
  if (!is.null(parsed$error)) stop(parsed$error$message)
  parsed$content[[1]]$text
}

provider_openai <- function(cfg, system_prompt, user_prompt) {
  parsed <- http_post_json(
    url     = "https://api.openai.com/v1/chat/completions",
    headers = list(
      "Authorization" = paste("Bearer", cfg$api_key),
      "Content-Type"  = "application/json"
    ),
    body_list = list(
      model    = cfg$model %||% "gpt-4o-mini",
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user",   content = user_prompt)
      )
    )
  )
  if (!is.null(parsed$error)) stop(parsed$error$message)
  parsed$choices[[1]]$message$content
}

provider_gemini <- function(cfg, system_prompt, user_prompt) {
  model_id <- cfg$model %||% "gemini-1.5-flash"
  url      <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model_id, ":generateContent?key=", cfg$api_key
  )
  parsed <- http_post_json(
    url     = url,
    headers = list("Content-Type" = "application/json"),
    body_list = list(
      contents = list(list(
        parts = list(list(text = paste0(system_prompt, "\n\n", user_prompt)))
      ))
    )
  )
  if (!is.null(parsed$error)) stop(parsed$error$message)
  parsed$candidates[[1]]$content$parts[[1]]$text
}

# ── Contexto y prompts ────────────────────────────────────────────────────

#' @export
build_system_prompt <- function(ctx) {
  ops_section <- if (length(ctx$ops) > 0)
    paste0("OPERACIONES YA APLICADAS:\n", ctx$ops_text, "\n\n") else ""
  sample_section <- if (!is.null(ctx$sample_text))
    paste0("MUESTRA DE DATOS (5 filas):\n", ctx$sample_text, "\n\n") else ""

  paste0(
    "Sos un asistente de análisis de datos especializado en R y tidyverse.\n",
    "Tu rol es ayudar a usuarios que vienen de SPSS a trabajar en R.\n\n",
    "DATASET ACTIVO: ", ctx$df_name, "\n",
    "Filas: ", ctx$nrow, " | Columnas: ", ctx$ncol, "\n\n",
    "VARIABLES DISPONIBLES:\n", ctx$schema_text, "\n\n",
    sample_section,
    ops_section,
    "REGLAS:\n",
    "1. Generá SOLO código R usando dplyr/tidyr/pipe nativo (|>).\n",
    "2. No inventes columnas que no estén en la lista de variables.\n",
    "3. Si el pedido es ambiguo, preguntá antes de asumir.\n",
    "4. Explicá brevemente qué hace el código.\n",
    "5. Encerrá el código R en bloque ```r ... ```.\n",
    "6. Si el usuario menciona sintaxis SPSS, traducila al equivalente en R."
  )
}

#' @export
build_ai_context <- function(df, df_name, ops, include_rows = FALSE) {
  meta <- get_metadata(df, df_name)
  schema_lines <- apply(meta$columns, 1, function(row) {
    paste0("  - ", row["name"], " (", row["visual"], ")")
  })
  sample_text <- if (include_rows && nrow(df) > 0) {
    paste(utils::capture.output(print(head(df, 5))), collapse = "\n")
  } else NULL

  ops_text <- if (length(ops) > 0) {
    paste(vapply(ops, function(o) paste0("  - ", o$label), character(1)),
          collapse = "\n")
  } else NULL

  list(
    df_name     = df_name,
    nrow        = meta$nrow,
    ncol        = meta$ncol,
    schema_text = paste(schema_lines, collapse = "\n"),
    sample_text = sample_text,
    ops         = ops,
    ops_text    = ops_text
  )
}

# ── Extracción de código ──────────────────────────────────────────────────

extract_r_code_block <- function(text) {
  pattern <- "```(?:r|R)?\\s*\\n?(.*?)```"
  matches <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  if (length(matches) == 0 || nchar(matches) == 0) return(NULL)
  code <- gsub("```(?:r|R)?\\s*\\n?|```", "", matches, perl = TRUE)
  trimws(code)
}

# ── null-coalesce ─────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && nchar(as.character(a)) > 0) a else b
