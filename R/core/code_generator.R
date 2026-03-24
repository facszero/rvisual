#' Generador de Código R
#'
#' Convierte un stack de operaciones (lista de objetos op_*) en
#' código R limpio, legible y reproducible usando tidyverse (dplyr/tidyr).
#'
#' Estrategia de generación:
#'   - Una operación por línea del pipe
#'   - Pipe nativo (|>) con R >= 4.1
#'   - Convenciones tidyverse: snake_case, nombres explícitos
#'   - Comentarios explicativos cuando la operación es compleja
#'   - Código legible para usuarios en transición desde SPSS

#' Generar código R desde nombre de dataset y stack de operaciones
#'
#' @param df_name  Nombre del objeto R que contiene el dataset
#' @param ops      Lista de operaciones (output de op_* functions)
#' @param assign_to Si no es NULL, el resultado se asigna a este nombre
#' @return String con código R formateado
#' @export
generate_code <- function(df_name, ops, assign_to = NULL) {
  if (length(ops) == 0) {
    return(glue::glue("# Aún no hay operaciones definidas sobre '{df_name}'."))
  }

  lines <- vapply(ops, op_to_code_line, character(1))

  # Construir pipe
  pipe_body <- paste(
    c(df_name, lines),
    collapse = " |>\n  "
  )

  # Asignación opcional
  if (!is.null(assign_to)) {
    code <- glue::glue("{assign_to} <- {pipe_body}")
  } else {
    code <- pipe_body
  }

  # Header de reproducibilidad
  header <- glue::glue(
    "# Código generado por RVisual\n",
    "# Dataset: {df_name}\n",
    "# Operaciones: {length(ops)}\n",
    "# Generado: {format(Sys.time(), '%Y-%m-%d %H:%M')}\n\n"
  )

  paste0(header, code)
}

# ── Convertidores operación → línea de código ─────────────────────────────

op_to_code_line <- function(op) {
  switch(op$type,
    "select"         = code_select(op$params),
    "filter"         = code_filter(op$params),
    "arrange"        = code_arrange(op$params),
    "group_summarise"= code_group_summarise(op$params),
    "mutate"         = code_mutate(op$params),
    "rename"         = code_rename(op$params),
    "recode"         = code_recode(op$params),
    "join"           = code_join(op$params),
    paste0("# operación desconocida: ", op$type)
  )
}

code_select <- function(p) {
  cols <- paste(p$cols, collapse = ", ")
  glue::glue("dplyr::select({cols})")
}

code_filter <- function(p) {
  # Determinar si value requiere comillas
  value_str <- format_value_for_code(p$value)
  glue::glue("dplyr::filter({p$col} {p$op} {value_str})")
}

code_arrange <- function(p) {
  if (p$desc) {
    glue::glue("dplyr::arrange(dplyr::desc({p$col}))")
  } else {
    glue::glue("dplyr::arrange({p$col})")
  }
}

code_group_summarise <- function(p) {
  group_str <- paste(p$group_cols, collapse = ", ")

  sum_exprs <- vapply(names(p$summary_fns), function(new_col) {
    fn_def  <- p$summary_fns[[new_col]]
    fn_name <- fn_def$fn
    src_col <- fn_def$col    # puede ser NULL si fn == "n"
    na_rm   <- fn_def$na_rm

    if (fn_name == "n" || is.null(src_col)) {
      as.character(glue::glue("{new_col} = dplyr::n()"))
    } else {
      na_str <- if (isTRUE(na_rm)) ", na.rm = TRUE" else ""
      as.character(glue::glue("{new_col} = {fn_name}({src_col}{na_str})"))
    }
  }, character(1))

  sum_str <- paste(sum_exprs, collapse = ",\n    ")
  glue::glue("dplyr::group_by({group_str}) |>\n  dplyr::summarise(\n    {sum_str},\n    .groups = 'drop'\n  )")
}

code_mutate <- function(p) {
  glue::glue("dplyr::mutate({p$new_col} = {p$expression})")
}

code_rename <- function(p) {
  glue::glue("dplyr::rename({p$new_name} = {p$old_name})")
}

code_recode <- function(p) {
  # Generar con case_when para mayor legibilidad
  cases <- vapply(names(p$mapping), function(old_val) {
    new_val <- p$mapping[[old_val]]
    old_str <- format_value_for_code(old_val)
    new_str <- format_value_for_code(new_val)
    glue::glue("{p$col} == {old_str} ~ {new_str}")
  }, character(1))

  cases_str <- paste(cases, collapse = ",\n      ")
  glue::glue(
    "dplyr::mutate({p$col} = dplyr::case_when(\n",
    "      {cases_str},\n",
    "      TRUE ~ {p$col}\n",
    "    ))"
  )
}

code_join <- function(p) {
  join_fn  <- paste0("dplyr::", p$type, "_join")
  by_str   <- if (length(p$by) == 1) {
    paste0('"', p$by, '"')
  } else {
    paste0('c(', paste(paste0('"', p$by, '"'), collapse = ', '), ')')
  }
  glue::glue("{join_fn}({p$right_df}, by = {by_str})")
}

# ── Utilitarios ───────────────────────────────────────────────────────────

format_value_for_code <- function(value) {
  # Si puede parsearse como número, no pone comillas
  if (!is.na(suppressWarnings(as.numeric(value)))) {
    as.character(value)
  } else {
    paste0('"', value, '"')
  }
}
