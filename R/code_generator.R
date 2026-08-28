#' Generador de Codigo R
#'
#' Convierte un stack de operaciones en codigo R limpio usando tidyverse.
#' Usa el pipe nativo \code{|>} disponible desde R 4.1.
#'
#' @name code_generator
#' @keywords internal
NULL

#' Generar codigo R desde un stack de operaciones
#'
#' @param df_name Nombre del objeto R que contiene el dataset.
#' @param ops Lista de operaciones generadas por las funciones \code{op_*}.
#' @param assign_to Si no es \code{NULL}, el resultado se asigna a este
#'   nombre de variable.
#' @param source Lista con metadatos del origen del dataset (ver
#'   \code{generate_load_step}). Si no es \code{NULL}, el codigo generado
#'   incluye el paso de carga correspondiente, de forma que el script sea
#'   ejecutable de forma standalone (sin depender de que el addin este
#'   activo ni de ningun entorno interno).
#' @return String con codigo R formateado y listo para ejecutar.
#' @keywords internal
generate_code <- function(df_name, ops, assign_to = NULL, source = NULL) {
  load_step <- generate_load_step(df_name, source)

  body_code <- if (length(ops) == 0) {
    paste0("# Sin operaciones definidas sobre '", df_name, "'.")
  } else {
    lines     <- vapply(ops, op_to_code_line, character(1))
    pipe_body <- paste(c(df_name, lines), collapse = " |>\n  ")
    if (!is.null(assign_to))
      as.character(glue::glue("{assign_to} <- {pipe_body}")) else pipe_body
  }

  header <- paste0(
    "# C\u00f3digo generado por RVisual\n",
    "# Dataset: ", df_name, "\n",
    "# Operaciones: ", length(ops), "\n",
    "# Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n"
  )

  code <- if (!is.null(load_step)) paste0(load_step, "\n\n", body_code) else body_code
  paste0(header, code)
}

#' Generar el paso de carga de datos para que el codigo sea standalone
#'
#' Convierte los metadatos de origen de un dataset (archivo local, tabla de
#' BD, u objeto que ya existia en el entorno del usuario) en la(s) linea(s)
#' de codigo R necesarias para reproducir esa carga de forma independiente
#' del addin.
#'
#' @param df_name Nombre del objeto R a crear.
#' @param source Lista con al menos el campo \code{kind}: \code{"archivo"},
#'   \code{"db"}, \code{"entorno"}, o \code{NULL} si se desconoce el origen.
#' @return String con el codigo de carga, o \code{NULL} si no aplica.
#' @keywords internal
generate_load_step <- function(df_name, source) {
  if (is.null(source) || is.null(source$kind)) return(NULL)

  switch(source$kind,

    "archivo" = {
      ext <- tolower(source$ext %||% "")
      ruta <- source$filename %||% paste0(df_name, ".", ext)
      linea <- switch(ext,
        csv = as.character(glue::glue(
          '{df_name} <- read.csv("{ruta}", sep = "{source$sep %||% ","}", ',
          'header = {if (isFALSE(source$header)) "FALSE" else "TRUE"}, ',
          'fileEncoding = "{source$encoding %||% "UTF-8"}", stringsAsFactors = FALSE)'
        )),
        xlsx = , xls = as.character(glue::glue(
          '{df_name} <- readxl::read_excel("{ruta}")'
        )),
        rds = as.character(glue::glue(
          '{df_name} <- readRDS("{ruta}")'
        )),
        sav = as.character(glue::glue(
          '{df_name} <- haven::read_sav("{ruta}")'
        )),
        as.character(glue::glue('{df_name} <- readRDS("{ruta}")  # TODO: verificar formato'))
      )
      paste0(
        "# Carga de datos - ajusta la ruta si el archivo no esta en el ",
        "directorio de trabajo\n", linea
      )
    },

    "db" = {
      # pw_mid: password no es el ultimo argumento (lleva coma final)
      # pw_last: password es el ultimo argumento (sin coma final, para no
      # generar una coma colgante antes del parentesis de cierre, invalida en R)
      pw_mid  <- '  password = rstudioapi::askForPassword("Contrasena de la base de datos"),\n'
      pw_last <- '  password = rstudioapi::askForPassword("Contrasena de la base de datos")\n'
      pwd_last <- '  PWD = rstudioapi::askForPassword("Contrasena de la base de datos")\n'
      conn_args <- switch(source$tipo,
        postgresql = as.character(glue::glue(
          "  RPostgres::Postgres(),\n",
          '  host = "{source$host}", port = {source$port}, dbname = "{source$dbname}",\n',
          '  user = "{source$user}",\n', pw_last
        )),
        mysql = as.character(glue::glue(
          "  RMySQL::MySQL(),\n",
          '  host = "{source$host}", port = {source$port}, dbname = "{source$dbname}",\n',
          '  user = "{source$user}",\n', pw_last
        )),
        sqlserver = as.character(glue::glue(
          "  odbc::odbc(),\n",
          '  Driver = "{source$driver_name %||% "ODBC Driver 17 for SQL Server"}",\n',
          '  Server = "{source$host},{source$port}", Database = "{source$dbname}",\n',
          '  UID = "{source$user}",\n', pwd_last
        )),
        oracle = as.character(glue::glue(
          "  ROracle::Oracle(),\n",
          '  username = "{source$user}",\n', pw_mid,
          '  dbname = "{source$oracle_service %||% source$host}"\n'
        )),
        sqlite = as.character(glue::glue(
          '  RSQLite::SQLite(),\n  dbname = "{source$filepath %||% ""}"\n'
        )),
        db2 = as.character(glue::glue(
          "  odbc::odbc(),\n",
          '  Driver = "{source$driver_name %||% "IBM DB2 ODBC DRIVER"}",\n',
          '  Database = "{source$dbname}", Hostname = "{source$host}", Port = {source$port},\n',
          '  UID = "{source$user}",\n', pwd_last
        )),
        '  # motor de BD no reconocido\n'
      )

      tabla   <- source$tabla %||% df_name
      limite  <- suppressWarnings(as.integer(source$limite %||% 0L))
      query <- if (!is.na(limite) && limite > 0) {
        as.character(glue::glue('DBI::dbGetQuery(con, "SELECT * FROM {tabla} ..."  ) # ajusta el LIMIT/ROWNUM a {limite} filas segun tu motor'))
      } else {
        as.character(glue::glue('DBI::dbReadTable(con, "{tabla}")'))
      }

      paste0(
        "# Conexion a base de datos - la contrasena NUNCA se guarda ni se ",
        "genera en el codigo,\n# se solicita en el momento de ejecutar\n",
        "con <- DBI::dbConnect(\n", conn_args, ")\n",
        df_name, " <- ", query, "\n",
        "DBI::dbDisconnect(con)"
      )
    },

    "entorno" = paste0(
      "# '", df_name, "' ya exist\u00eda en tu entorno de R antes de abrir RVisual.\n",
      "# Este script asume que sigue disponible con ese nombre; si no, ",
      "volve a crearlo/cargarlo antes de continuar."
    ),

    NULL
  )
}

# Nota: %||% ya esta definido en ai_interface.R y se reutiliza aqui
# (mismo namespace del paquete).

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
