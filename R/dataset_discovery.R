#' Descubrimiento de datasets y extracci\u00f3n de metadatos
#'
#' Funciones para detectar data.frames en el entorno global, extraer
#' metadatos estructurados y cargar archivos de datos.
#'
#' @name dataset_discovery
#' @keywords internal
NULL

#' Listar data.frames en el entorno global
#'
#' @return Lista de listas, cada una con \code{name}, \code{nrow}, \code{ncol}.
#' @keywords internal
discover_datasets <- function() {
  env    <- .GlobalEnv
  names_ <- ls(envir = env)
  Filter(Negate(is.null), lapply(names_, function(nm) {
    obj <- get(nm, envir = env)
    if (is.data.frame(obj)) list(name = nm, nrow = nrow(obj), ncol = ncol(obj))
    else NULL
  }))
}

#' Extraer metadatos de un data.frame
#'
#' Devuelve informaci\u00f3n sobre dimensiones y tipos de cada columna,
#' \u00fatil para construir el contexto del Asistente IA y el panel Explorador.
#'
#' @param df Data.frame a analizar.
#' @param name Nombre del objeto para display.
#' @return Lista con \code{name}, \code{nrow}, \code{ncol} y \code{columns}
#'   (data.frame con info de cada variable).
#' @keywords internal
get_metadata <- function(df, name = "dataset") {
  col_info <- lapply(names(df), function(col_name) {
    col      <- df[[col_name]]
    r_type   <- class(col)[1]
    vis_type <- classify_visual_type(col)
    n_unique <- length(unique(col))
    na_pct   <- round(mean(is.na(col)) * 100, 1)
    example  <- format_example_value(col)
    data.frame(name = col_name, type = r_type, visual = vis_type,
               unique = n_unique, na_pct = na_pct, example = example,
               stringsAsFactors = FALSE)
  })
  list(name = name, nrow = nrow(df), ncol = ncol(df),
       columns = do.call(rbind, col_info))
}

classify_visual_type <- function(col) {
  if (is.numeric(col))              return("Num\u00e9rica")
  if (is.logical(col))              return("L\u00f3gica")
  if (is.factor(col))               return("Categ\u00f3rica (factor)")
  if (inherits(col, "Date"))        return("Fecha")
  if (inherits(col, "POSIXct"))     return("Fecha y hora")
  if (is.character(col)) {
    if (length(unique(col)) <= 30)  return("Texto (pocos valores)")
    return("Texto libre")
  }
  "Otro"
}

format_example_value <- function(col) {
  non_na <- col[!is.na(col)]
  if (length(non_na) == 0) return("(vac\u00edo)")
  ex <- as.character(head(non_na, 1))
  if (nchar(ex) > 30) ex <- paste0(substr(ex, 1, 27), "...")
  ex
}

#' Cargar un archivo de datos
#'
#' @param path Ruta al archivo.
#' @param extension Extensi\u00f3n sin punto: "csv", "xlsx", "rds", "sav".
#' @return Data.frame o tibble con los datos cargados.
#' @keywords internal
load_file <- function(path, extension) {
  ext <- tolower(extension)
  switch(ext,
    "csv"  = readr::read_csv(path, show_col_types = FALSE),
    "tsv"  = readr::read_tsv(path, show_col_types = FALSE),
    "xlsx" = , "xls" = readxl::read_excel(path),
    "rds"  = readRDS(path),
    "sav"  = haven::read_sav(path),
    stop(glue::glue("Formato no soportado: .{ext}"))
  )
}
