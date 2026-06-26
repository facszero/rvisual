#' Generador de C\u00f3digo R
#'
#' Convierte un stack de operaciones en c\u00f3digo R limpio usando tidyverse.
#' Usa el pipe nativo \code{|>} disponible desde R 4.1.
#'
#' @name code_generator
#' @keywords internal
NULL

#' Generar c\u00f3digo R desde un stack de operaciones
#'
#' @param df_name Nombre del objeto R que contiene el dataset.
#' @param ops Lista de operaciones generadas por las funciones \code{op_*}.
#' @param assign_to Si no es \code{NULL}, el resultado se asigna a este
#'   nombre de variable.
#' @return String con c\u00f3digo R formateado y listo para ejecutar.
#' @keywords internal
generate_code <- function(df_name, ops, assign_to = NULL) {
  if (length(ops) == 0)
    return(glue::glue("# Sin operaciones definidas sobre '{df_name}'."))

  lines    <- vapply(ops, op_to_code_line, character(1))
  pipe_body <- paste(c(df_name, lines), collapse = " |>\n  ")

  code <- if (!is.null(assign_to))
    glue::glue("{assign_to} <- {pipe_body}") else pipe_body

  header <- paste0(
    "# C\u00f3digo generado por RVisual\n",
    "# Dataset: ", df_name, "\n",
    "# Operaciones: ", length(ops), "\n",
    "# Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n"
  )
  paste0(header, code)
}

op_to_code_line <- function(op) {
  switch(op$type,
    "select"          = code_select(op$params),
    "filter"          = code_filter(op$params),
    "arrange"         = code_arrange(op$params),
    "group_summarise" = code_group_summarise(op$params),
    "mutate"          = code_mutate(op$params),
    "rename"          = code_rename(op$params),
    "recode"          = code_recode(op$params),
    "join"            = code_join(op$params),
    paste0("# operaci\u00f3n desconocida: ", op$type)
  )
}

# Helper: envolver nombres de columna en backticks si tienen espacios o puntos
safe_col <- function(col) {
  if (grepl("^[0-9]|[^a-zA-Z0-9_]", col)) paste0("`", col, "`") else col
}

code_select <- function(p) {
  cols <- paste(vapply(p$cols, safe_col, character(1)), collapse = ", ")
  as.character(glue::glue("dplyr::select({cols})"))
}

code_filter <- function(p) {
  value_str <- format_value_for_code(p$value)
  col       <- safe_col(p$col)
  as.character(glue::glue("dplyr::filter({col} {p$op} {value_str})"))
}

code_arrange <- function(p) {
  col <- safe_col(p$col)
  if (p$desc) as.character(glue::glue("dplyr::arrange(dplyr::desc({col}))"))
  else        as.character(glue::glue("dplyr::arrange({col})"))
}

code_group_summarise <- function(p) {
  group_str <- paste(vapply(p$group_cols, safe_col, character(1)), collapse = ", ")
  sum_exprs <- vapply(names(p$summary_fns), function(new_col) {
    fn_def  <- p$summary_fns[[new_col]]
    fn_name <- fn_def$fn
    src_col <- if (!is.null(fn_def$col)) safe_col(fn_def$col) else NULL
    na_rm   <- fn_def$na_rm
    if (fn_name == "n" || is.null(src_col)) {
      as.character(glue::glue("{new_col} = dplyr::n()"))
    } else {
      na_str <- if (isTRUE(na_rm)) ", na.rm = TRUE" else ""
      as.character(glue::glue("{new_col} = {fn_name}({src_col}{na_str})"))
    }
  }, character(1))
  sum_str <- paste(sum_exprs, collapse = ",\n    ")
  if (length(p$group_cols) > 0) {
    # Con agrupacion: group_by |> summarise
    as.character(glue::glue(
      "dplyr::group_by({group_str}) |>\n  dplyr::summarise(\n    {sum_str},\n    .groups = 'drop'\n  )"
    ))
  } else {
    # Sin agrupacion: solo summarise (agregacion global)
    as.character(glue::glue(
      "dplyr::summarise(\n    {sum_str}\n  )"
    ))
  }
}

code_mutate <- function(p) {
  # Normalizar comillas tipograficas a comillas ASCII rectas
  expr <- p$expression
  expr <- gsub("\u201c|\u201d", '"', expr)
  expr <- gsub("\u2018|\u2019", "'", expr)
  col  <- safe_col(p$new_col)
  as.character(glue::glue("dplyr::mutate({col} = {expr})"))
}

code_rename <- function(p) {
  new <- safe_col(p$new_name)
  old <- safe_col(p$old_name)
  as.character(glue::glue("dplyr::rename({new} = {old})"))
}

code_recode <- function(p) {
  col   <- safe_col(p$col)
  cases <- vapply(names(p$mapping), function(old_val) {
    new_val <- p$mapping[[old_val]]
    # Normalizar comillas tipograficas en valores nuevos
    new_val <- gsub("\u201c|\u201d", '"', new_val)
    new_val <- gsub("\u2018|\u2019", "'", new_val)
    as.character(glue::glue(
      "{col} == {format_value_for_code(old_val)} ~ {format_value_for_code(new_val)}"
    ))
  }, character(1))
  cases_str <- paste(cases, collapse = ",\n      ")
  # TRUE ~ as.character() evita error de tipo mixto en case_when
  # cuando se mapea de numerico a texto
  as.character(glue::glue(
    "dplyr::mutate({col} = dplyr::case_when(\n      {cases_str},\n      TRUE ~ as.character({col})\n    ))"
  ))
}

code_join <- function(p) {
  join_fn <- paste0("dplyr::", p$type, "_join")
  by      <- p$by
  nms     <- names(by)
  if (!is.null(nms) && any(nzchar(nms) & nms != by)) {
    # dplyr: c("col_izquierda" = "col_derecha") -> name=izq, value=der
    pairs  <- paste0('"', nms, '" = "', by, '"')
    by_str <- paste0('c(', paste(pairs, collapse = ', '), ')')
  } else {
    by_str <- if (length(by) == 1) paste0('"', by, '"')
              else paste0('c(', paste(paste0('"', by, '"'), collapse = ', '), ')')
  }
  as.character(glue::glue("{join_fn}({p$right_df}, by = {by_str})"))
}

format_value_for_code <- function(value) {
  if (!is.na(suppressWarnings(as.numeric(value)))) as.character(value)
  else paste0('"', value, '"')
}
