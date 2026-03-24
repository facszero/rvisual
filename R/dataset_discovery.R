#' Descubrimiento de datasets, metadatos y carga de archivos

# ── Descubrimiento de datasets en entorno global ──────────────────────────

#' Listar data.frames disponibles en el entorno global
#'
#' @return Lista de listas con name, nrow, ncol por cada dataset
#' @export
discover_datasets <- function() {
  env    <- .GlobalEnv
  names_ <- ls(envir = env)

  datasets <- Filter(Negate(is.null), lapply(names_, function(nm) {
    obj <- get(nm, envir = env)
    if (is.data.frame(obj) || tibble::is_tibble(obj)) {
      list(name = nm, nrow = nrow(obj), ncol = ncol(obj))
    } else {
      NULL
    }
  }))

  datasets
}

# ── Metadatos y perfilado ─────────────────────────────────────────────────

#' Extraer metadatos estructurados de un data.frame
#'
#' @param df   Data.frame
#' @param name Nombre del objeto (para display)
#' @return Lista con nrow, ncol, columns (data.frame con info de cada columna)
#' @export
get_metadata <- function(df, name = "dataset") {
  col_info <- lapply(names(df), function(col_name) {
    col       <- df[[col_name]]
    r_type    <- class(col)[1]
    vis_type  <- classify_visual_type(col)
    n_unique  <- length(unique(col))
    na_pct    <- round(mean(is.na(col)) * 100, 1)
    example   <- format_example_value(col)

    data.frame(
      name    = col_name,
      type    = r_type,
      visual  = vis_type,
      unique  = n_unique,
      na_pct  = na_pct,
      example = example,
      stringsAsFactors = FALSE
    )
  })

  list(
    name    = name,
    nrow    = nrow(df),
    ncol    = ncol(df),
    columns = do.call(rbind, col_info)
  )
}

#' Clasificar tipo visual de una columna (para mostrar al usuario)
classify_visual_type <- function(col) {
  if (is.numeric(col))   return("Numérica")
  if (is.logical(col))   return("Lógica")
  if (is.factor(col))    return("Categórica (factor)")
  if (inherits(col, "Date"))     return("Fecha")
  if (inherits(col, "POSIXct")) return("Fecha y hora")
  if (is.character(col)) {
    n_unique <- length(unique(col))
    if (n_unique <= 30) return("Texto (pocos valores)")
    return("Texto libre")
  }
  "Otro"
}

format_example_value <- function(col) {
  non_na <- col[!is.na(col)]
  if (length(non_na) == 0) return("(vacío)")
  ex <- as.character(head(non_na, 1))
  if (nchar(ex) > 30) ex <- paste0(substr(ex, 1, 27), "...")
  ex
}

# ── Carga de archivos ─────────────────────────────────────────────────────

#' Cargar un archivo de datos según su extensión
#'
#' @param path      Ruta al archivo
#' @param extension Extensión sin punto: "csv", "xlsx", "rds", "sav"
#' @return Data.frame o tibble
#' @export
load_file <- function(path, extension) {
  ext <- tolower(extension)
  switch(ext,
    "csv"  = readr::read_csv(path, show_col_types = FALSE),
    "tsv"  = readr::read_tsv(path, show_col_types = FALSE),
    "xlsx" = ,
    "xls"  = readxl::read_excel(path),
    "rds"  = readRDS(path),
    "sav"  = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("El paquete 'haven' es necesario para leer archivos SPSS (.sav). Instalalo con: install.packages('haven')")
      }
      haven::read_sav(path)
    },
    stop(glue::glue("Formato no soportado: .{ext}. Formatos válidos: csv, xlsx, rds, sav"))
  )
}
