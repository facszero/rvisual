#' Motor de Operaciones Visuales — Modelo Interno
#'
#' Define la representación estructurada intermedia de las operaciones
#' que el usuario construye visualmente.
#'
#' En lugar de concatenar texto, cada operación es un objeto R con
#' campos tipados que permiten:
#'   - Generar código R limpio y reproducible
#'   - Reconstruir la UI desde el estado
#'   - Soportar historial y deshacer
#'   - Exportar/importar flujos de trabajo
#'
#' Estructura de una operación (lista R):
#' \describe{
#'   \item{type}{Tipo de operación: select, filter, arrange, group_summarise, mutate, rename, recode, join}
#'   \item{label}{Descripción legible para mostrar en UI}
#'   \item{params}{Lista nombrada con parámetros específicos del tipo}
#'   \item{created_at}{Timestamp de creación}
#' }

# ── Constructores de operaciones ──────────────────────────────────────────

#' Crear operación: select
#' @param cols Character vector de nombres de columnas a seleccionar
#' @export
op_select <- function(cols) {
  stopifnot(is.character(cols), length(cols) > 0)
  list(
    type       = "select",
    label      = paste("Seleccionar:", paste(cols, collapse = ", ")),
    params     = list(cols = cols),
    created_at = Sys.time()
  )
}

#' Crear operación: filter
#' @param col Nombre de columna
#' @param op  Operador: "==", "!=", ">", "<", ">=", "<="
#' @param value Valor de comparación
#' @export
op_filter <- function(col, op, value) {
  stopifnot(is.character(col), op %in% c("==", "!=", ">", "<", ">=", "<="))
  list(
    type       = "filter",
    label      = glue::glue("Filtrar: {col} {op} {value}"),
    params     = list(col = col, op = op, value = value),
    created_at = Sys.time()
  )
}

#' Crear operación: arrange
#' @param col Nombre de columna
#' @param desc Lógico: TRUE = descendente
#' @export
op_arrange <- function(col, desc = FALSE) {
  list(
    type       = "arrange",
    label      = glue::glue("Ordenar por {col}{if (desc) ' (desc)' else ''}"),
    params     = list(col = col, desc = desc),
    created_at = Sys.time()
  )
}

#' Crear operación: group_summarise
#' @param group_cols Character vector de columnas de agrupación
#' @param summary_fns Lista nombrada: nombre_nueva_col = list(fn, col_origen)
#' @export
op_group_summarise <- function(group_cols, summary_fns) {
  list(
    type       = "group_summarise",
    label      = glue::glue("Agrupar por: {paste(group_cols, collapse=', ')}"),
    params     = list(group_cols = group_cols, summary_fns = summary_fns),
    created_at = Sys.time()
  )
}

#' Crear operación: mutate (nueva variable)
#' @param new_col Nombre de la nueva columna
#' @param expression Expresión como string (será validada antes de generar código)
#' @export
op_mutate <- function(new_col, expression) {
  list(
    type       = "mutate",
    label      = glue::glue("Nueva variable: {new_col} = {expression}"),
    params     = list(new_col = new_col, expression = expression),
    created_at = Sys.time()
  )
}

#' Crear operación: rename
#' @param old_name Nombre actual de la columna
#' @param new_name Nuevo nombre
#' @export
op_rename <- function(old_name, new_name) {
  list(
    type       = "rename",
    label      = glue::glue("Renombrar: {old_name} → {new_name}"),
    params     = list(old_name = old_name, new_name = new_name),
    created_at = Sys.time()
  )
}

#' Crear operación: recode
#' @param col Columna a recodificar
#' @param mapping Lista nombrada: valor_original = valor_nuevo
#' @export
op_recode <- function(col, mapping) {
  list(
    type       = "recode",
    label      = glue::glue("Recodificar: {col} ({length(mapping)} valores)"),
    params     = list(col = col, mapping = mapping),
    created_at = Sys.time()
  )
}

#' Crear operación: join
#' @param right_df_name Nombre del segundo dataset
#' @param by_cols Vector de columnas clave (mismas en ambos)
#' @param join_type "left", "right", "inner", "full"
#' @export
op_join <- function(right_df_name, by_cols, join_type = "left") {
  list(
    type       = "join",
    label      = glue::glue("{join_type}_join con {right_df_name} por {paste(by_cols, collapse=', ')}"),
    params     = list(right_df = right_df_name, by = by_cols, type = join_type),
    created_at = Sys.time()
  )
}

# ── Aplicar stack de operaciones a un data.frame ──────────────────────────

#' Aplicar una lista de operaciones a un data.frame
#'
#' @param df Data.frame base
#' @param ops Lista de operaciones (output de op_* functions)
#' @return Data.frame transformado
#' @export
apply_operations <- function(df, ops) {
  if (length(ops) == 0) return(df)

  result <- df
  for (op in ops) {
    result <- apply_single_operation(result, op)
  }
  result
}

apply_single_operation <- function(df, op) {
  switch(op$type,
    "select" = {
      dplyr::select(df, dplyr::all_of(op$params$cols))
    },
    "filter" = {
      expr_text <- build_filter_expr(op$params$col, op$params$op, op$params$value)
      dplyr::filter(df, eval(parse(text = expr_text)))
    },
    "arrange" = {
      if (op$params$desc) {
        dplyr::arrange(df, dplyr::desc(.data[[op$params$col]]))
      } else {
        dplyr::arrange(df, .data[[op$params$col]])
      }
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
    "rename" = {
      dplyr::rename(df, !!op$params$new_name := !!op$params$old_name)
    },
    "recode" = {
      # TODO: implementar con dplyr::recode o case_when
      df
    },
    "join" = {
      right_df <- get(op$params$right_df, envir = .GlobalEnv)
      join_fn  <- switch(op$params$type,
        "left"  = dplyr::left_join,
        "right" = dplyr::right_join,
        "inner" = dplyr::inner_join,
        "full"  = dplyr::full_join,
        dplyr::left_join
      )
      join_fn(df, right_df, by = op$params$by)
    },
    df  # fallback: devolver sin cambios
  )
}

# ── Helpers internos ──────────────────────────────────────────────────────

build_filter_expr <- function(col, op, value) {
  # Determinar si value es numérico o string
  value_str <- if (is.numeric(suppressWarnings(as.numeric(value)))) {
    as.character(value)
  } else {
    paste0('"', value, '"')
  }
  paste0(".data[['", col, "']] ", op, " ", value_str)
}
