#' Motor de Operaciones Visuales
#'
#' Define la representacion estructurada intermedia de las operaciones
#' que el usuario construye visualmente en el Constructor.
#'
#' Cada operacion es una lista R con campos tipados:
#' \describe{
#'   \item{type}{Tipo: select, filter, arrange, group_summarise, mutate, rename, recode, join}
#'   \item{label}{Descripcion legible para mostrar en la UI}
#'   \item{params}{Lista nombrada con parametros especificos del tipo}
#'   \item{created_at}{Timestamp de creacion}
#' }
#'
#' @name operation_model
#' @keywords internal
NULL

#' Crear operacion: seleccionar columnas
#' @param cols Character vector con los nombres de columnas a conservar.
#' @return Lista de operacion con type="select".
#' @examples
#' op <- rvisual:::op_select(c("nombre", "edad", "ingreso"))
#' @keywords internal
op_select <- function(cols) {
  stopifnot(is.character(cols), length(cols) > 0)
  list(type = "select",
       label = paste("Seleccionar:", paste(cols, collapse = ", ")),
       params = list(cols = cols),
       created_at = Sys.time())
}

#' Crear operacion: filtrar registros
#' @param col Nombre de la columna a filtrar.
#' @param op Operador de comparacion: "==", "!=", ">", "<", ">=", "<=".
#' @param value Valor de comparacion.
#' @return Lista de operacion con type="filter".
#' @examples
#' op <- rvisual:::op_filter("anio", "==", 2024)
#' @keywords internal
op_filter <- function(col, op, value) {
  stopifnot(is.character(col), op %in% c("==", "!=", ">", "<", ">=", "<="))
  list(type = "filter",
       label = glue::glue("Filtrar: {col} {op} {value}"),
       params = list(col = col, op = op, value = value),
       created_at = Sys.time())
}

#' Crear operacion: ordenar
#' @param col Nombre de la columna por la que ordenar.
#' @param desc Logico. Si \code{TRUE} ordena de forma descendente.
#' @return Lista de operacion con type="arrange".
#' @keywords internal
op_arrange <- function(col, desc = FALSE) {
  list(type = "arrange",
       label = glue::glue("Ordenar por {col}{if (desc) ' (desc)' else ''}"),
       params = list(col = col, desc = desc),
       created_at = Sys.time())
}

#' Crear operacion: agrupar y resumir
#' @param group_cols Character vector de columnas de agrupacion.
#' @param summary_fns Lista nombrada donde cada elemento define una
#'   funcion de resumen: \code{list(fn, col, na_rm)}.
#' @return Lista de operacion con type="group_summarise".
#' @keywords internal
op_group_summarise <- function(group_cols, summary_fns) {
  label_grp <- if (length(group_cols) > 0)
    glue::glue("Agrupar por: {paste(group_cols, collapse=', ')}")
  else
    "Resumir (sin agrupacion)"
  list(type = "group_summarise",
       label = label_grp,
       params = list(group_cols = group_cols, summary_fns = summary_fns),
       created_at = Sys.time())
}

#' Crear operacion: nueva variable (mutate)
#' @param new_col Nombre de la nueva columna.
#' @param expression Expresion R como string (ej: "Precio * Cantidad").
#' @return Lista de operacion con type="mutate".
#' @keywords internal
op_mutate <- function(new_col, expression) {
  list(type = "mutate",
       label = glue::glue("Nueva variable: {new_col} = {expression}"),
       params = list(new_col = new_col, expression = expression),
       created_at = Sys.time())
}

#' Crear operacion: renombrar columna
#' @param old_name Nombre actual de la columna.
#' @param new_name Nuevo nombre.
#' @return Lista de operacion con type="rename".
#' @keywords internal
op_rename <- function(old_name, new_name) {
  list(type = "rename",
       label = glue::glue("Renombrar: {old_name} \u2192 {new_name}"),
       params = list(old_name = old_name, new_name = new_name),
       created_at = Sys.time())
}

#' Crear operacion: recodificar valores
#' @param col Nombre de la columna a recodificar.
#' @param mapping Lista nombrada con los reemplazos:
#'   \code{list(valor_original = valor_nuevo)}.
#' @return Lista de operacion con type="recode".
#' @keywords internal
op_recode <- function(col, mapping) {
  list(type = "recode",
       label = glue::glue("Recodificar: {col} ({length(mapping)} valores)"),
       params = list(col = col, mapping = mapping),
       created_at = Sys.time())
}

#' Crear operacion: unir tablas
#' @param right_df_name Nombre del segundo data.frame: uno cargado en
#'   RVisual o uno del entorno global de R (ver \code{rvisual_find_dataset}).
#' @param by_cols Character vector con las columnas clave del join. Si tiene
#'   nombres, el nombre es la columna de la tabla izquierda y el valor la de
#'   la tabla derecha.
#' @param join_type Tipo de join: "left", "right", "inner", "full".
#' @param right_cols Columnas (no clave) a traer de la tabla derecha.
#'   \code{NULL} trae todas.
#' @param key_casts Conversiones de tipo de las claves, tal como las devuelve
#'   \code{join_key_check}: lista de \code{list(side, col)}; la columna se
#'   convierte a numero. \code{NULL} no convierte nada.
#' @param right_source Origen del segundo dataset (ver
#'   \code{generate_load_step}), para incluir su paso de carga en el codigo.
#' @return Lista de operacion con type="join".
#' @keywords internal
op_join <- function(right_df_name, by_cols, join_type = "left",
                    right_cols = NULL, key_casts = NULL, right_source = NULL) {
  list(type = "join",
       label = glue::glue("{join_type}_join con {right_df_name} por {paste(by_cols, collapse=', ')}"),
       params = list(right_df = right_df_name, by = by_cols, type = join_type,
                     right_cols = right_cols, key_casts = key_casts,
                     right_source = right_source),
       created_at = Sys.time())
}

#' Verificar que las claves de un join tengan tipos compatibles
#'
#' Compara el tipo de cada par de columnas clave. Si una es numerica y la
#' otra es texto con solo numeros (por ejemplo "0611100701"), propone
#' convertir la de texto a numero. Cualquier otra combinacion distinta se
#' informa como incompatible.
#'
#' @param left,right Data.frames izquierdo y derecho.
#' @param by Character vector de claves (con nombres = columnas izquierdas).
#' @return Lista con \code{ok} (logico), \code{casts} (lista de
#'   \code{list(side, col)}) y \code{messages} (texto para el usuario).
#' @keywords internal
join_key_check <- function(left, right, by) {
  lcols <- if (is.null(names(by))) by else ifelse(nzchar(names(by)), names(by), by)
  rcols <- unname(by)
  familia <- function(x) {
    if (is.factor(x) || is.character(x)) "texto"
    else if (inherits(x, c("Date", "POSIXt"))) "fecha"
    else if (is.logical(x)) "logico"
    else if (is.numeric(x)) "n\u00famero"
    else class(x)[1]
  }
  solo_numeros <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(trimws(x))]
    length(x) > 0 &&
      all(grepl("^\\s*[-+]?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?\\s*$", x))
  }
  ok <- TRUE
  casts <- list()
  msgs <- character(0)
  for (i in seq_along(lcols)) {
    lc <- lcols[i]
    rc <- rcols[i]
    if (!lc %in% names(left)) {
      ok <- FALSE
      msgs <- c(msgs, paste0("La columna '", lc, "' no est\u00e1 en la tabla izquierda."))
      next
    }
    if (!rc %in% names(right)) {
      ok <- FALSE
      msgs <- c(msgs, paste0("La columna '", rc, "' no est\u00e1 en la tabla derecha."))
      next
    }
    fl <- familia(left[[lc]])
    fr <- familia(right[[rc]])
    if (identical(fl, fr)) next
    if (setequal(c(fl, fr), c("n\u00famero", "texto"))) {
      lado <- if (fl == "texto") "left" else "right"
      col  <- if (lado == "left") lc else rc
      vals <- if (lado == "left") left[[lc]] else right[[rc]]
      if (solo_numeros(vals)) {
        casts[[length(casts) + 1]] <- list(side = lado, col = col)
        msgs <- c(msgs, paste0(
          "'", lc, "' es ", fl, " y '", rc, "' es ", fr, ": se convertir\u00e1 '",
          col, "' (tabla ", if (lado == "left") "izquierda" else "derecha",
          ") a n\u00famero."))
        next
      }
    }
    ok <- FALSE
    msgs <- c(msgs, paste0(
      "'", lc, "' es ", fl, " y '", rc, "' es ", fr,
      ": no se pueden unir. Convert\u00ed una de las dos con 'Crear variable'."))
  }
  list(ok = ok, casts = casts, messages = msgs)
}

# Aplica las conversiones de clave de un lado del join
join_apply_casts <- function(df, casts, side) {
  for (cs in casts) {
    if (identical(cs$side, side) && cs$col %in% names(df))
      df[[cs$col]] <- as.numeric(as.character(df[[cs$col]]))
  }
  df
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
      grp_df <- if (length(p$group_cols) > 0)
        dplyr::group_by(df, dplyr::across(dplyr::all_of(p$group_cols)))
      else
        df
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
    "recode"  = {
      col     <- op$params$col
      mapping <- op$params$mapping
      vals    <- df[[col]]
      result  <- as.character(vals)  # convertir a character para uniformidad
      for (old_val in names(mapping)) {
        new_val <- mapping[[old_val]]
        # Comparar numerico si corresponde
        if (!is.na(suppressWarnings(as.numeric(old_val))) &&
            is.numeric(vals)) {
          result[vals == as.numeric(old_val)] <- new_val
        } else {
          result[as.character(vals) == old_val] <- new_val
        }
      }
      df[[col]] <- result
      df
    },
    "join" = {
      p        <- op$params
      right_df <- rvisual_find_dataset(p$right_df)
      if (!is.null(p$right_cols)) {
        keep     <- unique(c(unname(p$by), p$right_cols))
        right_df <- right_df[, intersect(keep, names(right_df)), drop = FALSE]
      }
      df       <- join_apply_casts(df, p$key_casts, "left")
      right_df <- join_apply_casts(right_df, p$key_casts, "right")
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
