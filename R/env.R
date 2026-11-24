# env.R - Entorno interno de almacenamiento de datasets
# RVisual - Addin RStudio para usuarios de SPSS
#
# Los datasets activos durante una sesion de RVisual se almacenan en un
# entorno propio del paquete (no en .GlobalEnv), para evitar sobrescribir
# objetos del usuario y no incurrir en el NOTE de R CMD check sobre
# "assignment to .GlobalEnv" (ver discusion CRAN, revision de Uwe Ligges).
#
# La reproducibilidad del codigo generado -que antes dependia de que el
# dataset ya existiera en .GlobalEnv con ese nombre- ahora se resuelve de
# otra forma: el codigo generado incluye su propio "paso de carga" (lectura
# de archivo o conexion a BD), por lo que es standalone y no depende de
# este entorno interno ni de que el addin siga activo.
# Ver code_generator.R::generate_load_step().

.rvisual_env <- new.env(parent = emptyenv())

#' Entorno interno donde RVisual almacena los datasets activos durante la
#' sesion del addin.
#'
#' @return El entorno interno (`environment`) usado para `assign()`/`get()`
#'   de datasets mientras la app Shiny esta activa. Nunca es `.GlobalEnv`.
#' @keywords internal
rvisual_env <- function() {
  .rvisual_env
}

# -- Origen de los datasets y busqueda por nombre -------------------------
# El modulo Dataset guarda el origen de cada dataset (archivo, BD, entorno)
# en un reactiveValues local. Se replica aqui para que otros modulos (por
# ejemplo el join del Constructor) puedan generar el paso de carga de un
# segundo dataset sin depender del modulo Dataset.

.rvisual_sources <- new.env(parent = emptyenv())

#' Registrar los origenes de los datasets cargados
#'
#' @param sources Lista nombrada de metadatos de origen (ver
#'   \code{generate_load_step}).
#' @return Invisible \code{NULL}.
#' @keywords internal
rvisual_set_sources <- function(sources) {
  rm(list = ls(envir = .rvisual_sources, all.names = TRUE),
     envir = .rvisual_sources)
  for (nm in names(sources)) {
    if (!is.null(sources[[nm]])) assign(nm, sources[[nm]], envir = .rvisual_sources)
  }
  invisible(NULL)
}

#' Origen registrado de un dataset
#'
#' @param name Nombre del dataset.
#' @return Lista de metadatos de origen, o \code{NULL} si no se conoce.
#' @keywords internal
rvisual_source <- function(name) {
  if (is.character(name) && length(name) == 1 &&
      exists(name, envir = .rvisual_sources, inherits = FALSE))
    get(name, envir = .rvisual_sources, inherits = FALSE)
  else NULL
}

#' Buscar un dataset por nombre
#'
#' Busca primero entre los datasets cargados en RVisual y despues en el
#' entorno global de R.
#'
#' @param name Nombre del dataset.
#' @param global Entorno global de R (parametro para los tests).
#' @return El data.frame. Error si no existe en ninguno de los dos lugares.
#' @keywords internal
rvisual_find_dataset <- function(name, global = globalenv()) {
  if (exists(name, envir = rvisual_env(), inherits = FALSE)) {
    df <- get(name, envir = rvisual_env(), inherits = FALSE)
    if (is.data.frame(df)) return(df)
  }
  if (exists(name, envir = global, inherits = FALSE)) {
    df <- get(name, envir = global, inherits = FALSE)
    if (is.data.frame(df)) return(df)
  }
  stop("No se encontr\u00f3 el dataset '", name,
       "' ni entre los cargados en RVisual ni en el entorno global de R.",
       call. = FALSE)
}

#' Datasets disponibles para unir
#'
#' @return Lista nombrada con dos grupos (para \code{selectInput}): los
#'   datasets cargados en RVisual y los data.frames del entorno global que
#'   no fueron cargados en RVisual. Los grupos vacios se omiten.
#' @param global Entorno global de R (parametro para los tests).
#' @keywords internal
rvisual_join_candidates <- function(global = globalenv()) {
  es_df <- function(nm, env) {
    tryCatch(is.data.frame(get(nm, envir = env, inherits = FALSE)),
             error = function(e) FALSE)
  }
  propios <- ls(envir = rvisual_env())
  propios <- sort(propios[vapply(propios, es_df, logical(1), env = rvisual_env())])
  globales <- ls(envir = global)
  globales <- globales[vapply(globales, es_df, logical(1), env = global)]
  globales <- sort(setdiff(globales, propios))
  out <- list()
  if (length(propios) > 0)  out[["Cargados en RVisual"]] <- as.list(propios)
  if (length(globales) > 0) out[["Entorno global de R"]] <- as.list(globales)
  out
}
