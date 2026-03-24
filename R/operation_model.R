#' Motor de Operaciones Visuales
#'
#' Define la representaci\u00f3n estructurada intermedia de las operaciones
#' que el usuario construye visualmente en el Constructor.
#'
#' Cada operaci\u00f3n es una lista R con campos tipados:
#' \describe{
#'   \item{type}{Tipo: select, filter, arrange, group_summarise, mutate, rename, recode, join}
#'   \item{label}{Descripci\u00f3n legible para mostrar en la UI}
#'   \item{params}{Lista nombrada con par\u00e1metros espec\u00edficos del tipo}
#'   \item{created_at}{Timestamp de creaci\u00f3n}
#' }
#'
#' @name operation_model
#' @keywords internal
NULL

#' Crear operaci\u00f3n: seleccionar columnas
#' @param cols Character vector con los nombres de columnas a conservar.
#' @return Lista de operaci\u00f3n con type="select".
#' @examples
#' op <- op_select(c("nombre", "edad", "ingreso"))
#' @keywords internal
op_select <- function(cols) {
  stopifnot(is.character(cols), length(cols) > 0)
  list(type = "select",
       label = paste("Seleccionar:", paste(cols, collapse = ", ")),
       params = list(cols = cols),
       created_at = Sys.time())
}

#' Crear operaci\u00f3n: filtrar registros
#' @param col Nombre de la columna a filtrar.
#' @param op Operador de comparaci\u00f3n: "==", "!=", ">", "<", ">=", "<=".
#' @param value Valor de comparaci\u00f3n.
#' @return Lista de operaci\u00f3n con type="filter".
#' @examples
#' op <- op_filter("anio", "==", 2024)
#' @keywords internal
op_filter <- function(col, op, value) {
  stopifnot(is.character(col), op %in% c("==", "!=", ">", "<", ">=", "<="))
  list(type = "filter",
       label = glue::glue("Filtrar: {col} {op} {value}"),
       params = list(col = col, op = op, value = value),
       created_at = Sys.time())
}

#' Crear operaci\u00f3n: ordenar
#' @param col Nombre de la columna por la que ordenar.
#' @param desc L\u00f3gico. Si \code{TRUE} ordena de forma descendente.
#' @return Lista de operaci\u00f3n con type="arrange".
#' @keywords internal
op_arrange <- function(col, desc = FALSE) {
  list(type = "arrange",
       label = glue::glue("Ordenar por {col}{if (desc) ' (desc)' else ''}"),
       params = list(col = col, desc = desc),
       created_at = Sys.time())
}

#' Crear operaci\u00f3n: agrupar y resumir
#' @param group_cols Character vector de columnas de agrupaci\u00f3n.
#' @param summary_fns Lista nombrada donde cada elemento define una
#'   funci\u00f3n de resumen: \code{list(fn, col, na_rm)}.
#' @return Lista de operaci\u00f3n con type="group_summarise".
#' @keywords internal
op_group_summarise <- function(group_cols, summary_fns) {
  list(type = "group_summarise",
       label = glue::glue("Agrupar por: {paste(group_cols, collapse=', ')}"),
       params = list(group_cols = group_cols, summary_fns = summary_fns),
       created_at = Sys.time())
}

#' Crear operaci\u00f3n: nueva variable (mutate)
#' @param new_col Nombre de la nueva columna.
#' @param expression Expresi\u00f3n R como string (ej: "Precio * Cantidad").
#' @return Lista de operaci\u00f3n con type="mutate".
#' @keywords internal
op_mutate <- function(new_col, expression) {
  list(type = "mutate",
       label = glue::glue("Nueva variable: {new_col} = {expression}"),
       params = list(new_col = new_col, expression = expression),
       created_at = Sys.time())
}

#' Crear operaci\u00f3n: renombrar columna
#' @param old_name Nombre actual de la columna.
#' @param new_name Nuevo nombre.
#' @return Lista de operaci\u00f3n con type="rename".
#' @keywords internal
op_rename <- function(old_name, new_name) {
  list(type = "rename",
       label = glue::glue("Renombrar: {old_name} \u2192 {new_name}"),
       params = list(old_name = old_name, new_name = new_name),
       created_at = Sys.time())
}

#' Crear operaci\u00f3n: recodificar valores
#' @param col Nombre de la columna a recodificar.
#' @param mapping Lista nombrada con los reemplazos:
#'   \code{list(valor_original = valor_nuevo)}.
#' @return Lista de operaci\u00f3n con type="recode".
#' @keywords internal
op_recode <- function(col, mapping) {
  list(type = "recode",
       label = glue::glue("Recodificar: {col} ({length(mapping)} valores)"),
       params = list(col = col, mapping = mapping),
       created_at = Sys.time())
}

#' Crear operaci\u00f3n: unir tablas
#' @param right_df_name Nombre del segundo data.frame (debe existir en
#'   \code{.GlobalEnv}).
#' @param by_cols Character vector con las columnas clave del join.
#' @param join_type Tipo de join: "left", "right", "inner", "full".
#' @return Lista de operaci\u00f3n con type="join".
#' @keywords internal
op_join <- function(right_df_name, by_cols, join_type = "left") {
  list(type = "join",
       label = glue::glue("{join_type}_join con {right_df_name} por {paste(by_cols, collapse=', ')}"),
       params = list(right_df = right_df_name, by = by_cols, type = join_type),
       created_at = Sys.time())
}

#' Aplicar stack de operaciones a un data.frame
#'
#' Ejecuta secuencialmente una lista de operaciones sobre un data.frame,
#' retornando el resultado transformado.
#'
#' @param df Data.frame base.
#' @param ops Lista de operaciones generadas por las funciones \code{op_*}.
#' @return Data.frame transformado.
#' @keywords internal
apply_operations <- function(df, ops) {
  if (length(ops) == 0) return(df)
  result <- df
  for (op in ops) result <- apply_single_operation(result, op)
  result
}

apply_single_operation <- function(df, op) {
  switch(op$type,
    "select" = dplyr::select(df, dplyr::all_of(op$params$cols)),
    "filter" = {
      expr_text <- build_filter_expr(op$params$col, op$params$op, op$params$value)
      dplyr::filter(df, eval(parse(text = expr_text)))
    },
    "arrange" = {
      if (op$params$desc)
        dplyr::arrange(df, dplyr::desc(.data[[op$params$col]]))
      else
        dplyr::arrange(df, .data[[op$params$col]])
    },
    "group_summarise" = {
      p      <- op$params
      grp_df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(p$group_cols)))
      exprs  <- lapply(names(p$summary_fns), function(new_col) {
        fn_def  <- p$summary_fns[[new_col]]
        fn_name <- fn_def$fn
        src_col <- fn_def$col
        na_rm   <- isTRUE(fn_def$na_rm)
        if (fn_name == "n" || is.null(src_col)) {
          rlang::expr(dplyr::n())
        } else {
          fn <- match.fun(fn_name)
          if (na_rm) rlang::expr((!!fn)(.data[[!!src_col]], na.rm = TRUE))
          else       rlang::expr((!!fn)(.data[[!!src_col]]))
        }
      })
      names(exprs) <- names(p$summary_fns)
      dplyr::summarise(grp_df, !!!exprs, .groups = "drop")
    },
    "mutate" = {
      df[[op$params$new_col]] <- eval(parse(text = op$params$expression), envir = df)
      df
    },
    "rename"  = dplyr::rename(df, !!op$params$new_name := !!op$params$old_name),
    "recode"  = df,  # aplicado v\u00eda code_generator
    "join" = {
      right_df <- get(op$params$right_df, envir = .GlobalEnv)
      join_fn  <- switch(op$params$type,
        "left"  = dplyr::left_join,  "right" = dplyr::right_join,
        "inner" = dplyr::inner_join, "full"  = dplyr::full_join,
        dplyr::left_join)
      join_fn(df, right_df, by = op$params$by)
    },
    df
  )
}

build_filter_expr <- function(col, op, value) {
  value_str <- if (!is.na(suppressWarnings(as.numeric(value))))
    as.character(value) else paste0('"', value, '"')
  paste0(".data[['", col, "']] ", op, " ", value_str)
}
