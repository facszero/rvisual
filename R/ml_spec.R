#' Especificacion de un modelado (ml_spec)
#'
#' Representacion estructurada intermedia del panel Modelado, analoga a
#' las operaciones de \code{operation_model.R} pero separada de ellas: un
#' modelo no es una transformacion del pipeline de datos.
#'
#' La interfaz produce una \code{ml_spec}; la generacion de codigo, la
#' ejecucion y los tests se derivan de ella. Campos:
#' \describe{
#'   \item{version}{Version del esquema (entero). Permite extenderlo en
#'     fases siguientes (pesos, estratos) sin romper especificaciones
#'     guardadas.}
#'   \item{task}{"regression", "binary" o "multiclass".}
#'   \item{target}{Nombre de la variable objetivo.}
#'   \item{predictors}{Nombres de los predictores.}
#'   \item{positive}{Clase positiva (solo "binary").}
#'   \item{split}{\code{list(test_prop, seed, stratify)}.}
#'   \item{na_action}{"omit" o "impute_simple".}
#'   \item{cv}{\code{list(folds)}.}
#'   \item{models}{Lista de \code{ml_model()}.}
#'   \item{metrics}{Metricas a reportar.}
#'   \item{final}{Motor elegido por el analista (o \code{NULL}).}
#' }
#'
#' @name ml_spec_model
#' @keywords internal
NULL

#' Version actual del esquema ml_spec
#' @return Entero.
#' @keywords internal
ml_spec_version <- function() 1L

#' Inferir el tipo de tarea a partir de la variable objetivo
#'
#' Reglas: numerica con mas de dos valores distintos -> "regression";
#' dos valores distintos (de cualquier tipo) -> "binary"; factor,
#' caracter o logico con 3 a \code{max_classes} valores -> "multiclass".
#'
#' @param x Vector con la variable objetivo.
#' @param max_classes Maximo de clases admitido para clasificacion.
#' @return Uno de \code{ml_tasks()}, o \code{NA_character_} si la variable
#'   no sirve como objetivo (constante, vacia o con demasiadas clases).
#' @examples
#' rvisual:::ml_detect_task(mtcars$mpg)
#' rvisual:::ml_detect_task(iris$Species)
#' @keywords internal
ml_detect_task <- function(x, max_classes = 50L) {
  values <- unique(x[!is.na(x)])
  n_distinct <- length(values)
  if (n_distinct < 2) return(NA_character_)
  if (n_distinct == 2) return("binary")
  if (is.numeric(x) && !is.factor(x)) return("regression")
  if (n_distinct <= max_classes) return("multiclass")
  NA_character_
}

#' Definir un modelo candidato
#' @param engine Identificador de motor (ver \code{ml_engine_catalog()}).
#' @param params Lista nombrada de hiperparametros; los no indicados toman
#'   el valor por defecto del catalogo.
#' @return Lista \code{list(engine, params)}.
#' @examples
#' rvisual:::ml_model("ranger", list(num.trees = 300L))
#' @keywords internal
ml_model <- function(engine, params = list()) {
  defaults <- ml_engine_defaults(engine)
  unknown <- setdiff(names(params), names(defaults))
  if (length(unknown) > 0) {
    stop("Hiperpar\u00e1metros desconocidos para ", engine, ": ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  merged <- defaults
  for (nm in names(params)) merged[nm] <- list(params[[nm]])
  list(engine = engine, params = merged)
}

#' Construir una especificacion de modelado
#'
#' @param target Nombre de la variable objetivo.
#' @param predictors Character vector con los predictores.
#' @param task Tarea; ver \code{ml_tasks()}.
#' @param positive Clase positiva (obligatoria si \code{task = "binary"}).
#' @param models Lista de \code{ml_model()}. Por defecto: linea base y
#'   todos los motores del catalogo que admiten la tarea y cuyo paquete
#'   esta instalado.
#' @param test_prop Proporcion reservada para test.
#' @param seed Semilla para la particion, los folds y los modelos.
#' @param stratify Estratificar por la variable objetivo. Por defecto
#'   \code{TRUE} en clasificacion y \code{FALSE} en regresion.
#' @param na_action "omit" (eliminar filas con NA) o "impute_simple".
#' @param folds Numero de folds de validacion cruzada.
#' @param metrics Metricas a reportar; por defecto las de la tarea.
#' @return Lista con la estructura descrita en \code{ml_spec_model}.
#' @examples
#' spec <- rvisual:::ml_spec("mpg", c("wt", "hp"), task = "regression")
#' @keywords internal
ml_spec <- function(target, predictors, task, positive = NULL,
                    models = NULL, test_prop = 0.25, seed = 123L,
                    stratify = NULL, na_action = "omit", folds = 5L,
                    metrics = NULL) {
  if (is.null(models)) {
    engines <- Filter(ml_engine_available, ml_engines_for_task(task))
    models <- lapply(engines, ml_model)
  }
  if (is.null(stratify)) stratify <- task != "regression"
  if (is.null(metrics)) metrics <- ml_metrics_for_task(task)
  list(
    version    = ml_spec_version(),
    task       = task,
    target     = target,
    predictors = predictors,
    positive   = positive,
    split      = list(test_prop = test_prop, seed = seed, stratify = stratify),
    na_action  = na_action,
    cv         = list(folds = folds),
    models     = models,
    metrics    = metrics,
    final      = NULL
  )
}

#' Validar una especificacion de modelado
#'
#' Verifica la estructura y, si se pasa \code{data}, la coherencia con
#' el dataset. No lanza errores: devuelve los problemas encontrados para
#' que la interfaz los muestre.
#'
#' @param spec Lista producida por \code{ml_spec()}.
#' @param data Data.frame opcional contra el cual validar columnas y tipos.
#' @param check_packages Verificar que los paquetes de los motores esten
#'   instalados.
#' @return Lista con \code{valid} (logico), \code{errors} y
#'   \code{warnings} (character vectors).
#' @keywords internal
ml_spec_validate <- function(spec, data = NULL, check_packages = TRUE) {
  errors <- character(0)
  warns  <- character(0)
  err  <- function(...) errors <<- c(errors, paste0(...))
  warn <- function(...) warns  <<- c(warns, paste0(...))
  is_count <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x == round(x)

  # -- Estructura -----------------------------------------------------
  if (!is.list(spec)) {
    return(list(valid = FALSE, errors = "La especificaci\u00f3n no es una lista.",
                warnings = character(0)))
  }
  if (!identical(spec$version, ml_spec_version())) {
    err("Versi\u00f3n de especificaci\u00f3n no soportada: ", format(spec$version))
  }
  task_ok <- is.character(spec$task) && length(spec$task) == 1 &&
    spec$task %in% ml_tasks()
  if (!task_ok) err("Tarea inv\u00e1lida: ", format(spec$task))

  if (!is.character(spec$target) || length(spec$target) != 1 || !nzchar(spec$target)) {
    err("Debe indicarse una \u00fanica variable objetivo.")
  }
  if (!is.character(spec$predictors) || length(spec$predictors) == 0) {
    err("Debe indicarse al menos un predictor.")
  } else {
    if (anyDuplicated(spec$predictors)) err("Hay predictores repetidos.")
    if (is.character(spec$target) && any(spec$predictors %in% spec$target)) {
      err("La variable objetivo no puede estar entre los predictores.")
    }
  }

  if (task_ok && spec$task == "binary") {
    if (!is.character(spec$positive) || length(spec$positive) != 1) {
      err("En clasificaci\u00f3n binaria debe indicarse la clase positiva.")
    }
  }

  # -- Particion, CV y NA ----------------------------------------------
  tp <- spec$split$test_prop
  if (!is.numeric(tp) || length(tp) != 1 || is.na(tp) || tp <= 0 || tp > 0.5) {
    err("La proporci\u00f3n de test debe estar entre 0 y 0,5.")
  }
  if (!is_count(spec$split$seed)) err("La semilla debe ser un n\u00famero entero.")
  if (!is.logical(spec$split$stratify) || length(spec$split$stratify) != 1 ||
      is.na(spec$split$stratify)) {
    err("'stratify' debe ser TRUE o FALSE.")
  } else if (task_ok && spec$task == "regression" && isTRUE(spec$split$stratify)) {
    warn("La estratificaci\u00f3n por la variable objetivo se ignora en regresi\u00f3n.")
  }
  if (!is_count(spec$cv$folds) || spec$cv$folds < 2 || spec$cv$folds > 20) {
    err("El n\u00famero de folds debe ser un entero entre 2 y 20.")
  }
  if (!identical(spec$na_action, "omit") && !identical(spec$na_action, "impute_simple")) {
    err("Tratamiento de NA inv\u00e1lido: ", format(spec$na_action))
  }

  # -- Modelos ---------------------------------------------------------
  catalog <- ml_engine_catalog()
  if (!is.list(spec$models) || length(spec$models) == 0) {
    err("Debe seleccionarse al menos un modelo.")
  } else {
    engines <- vapply(spec$models, function(m) {
      if (is.character(m$engine) && length(m$engine) == 1) m$engine else NA_character_
    }, character(1))
    if (anyDuplicated(stats::na.omit(engines))) err("Hay modelos repetidos.")
    for (i in seq_along(spec$models)) {
      e <- engines[i]
      if (is.na(e) || !e %in% names(catalog)) {
        err("Motor desconocido: ", format(spec$models[[i]]$engine))
        next
      }
      def <- catalog[[e]]
      if (task_ok && !spec$task %in% def$tasks) {
        err("El modelo '", def$label, "' no admite la tarea '", spec$task, "'.")
      }
      if (check_packages && !ml_engine_available(e)) {
        err("Falta instalar el paquete '", def$package, "' para usar '", def$label, "'.")
      }
      params <- spec$models[[i]]$params
      for (nm in names(params)) {
        p <- def$params[[nm]]
        v <- params[[nm]]
        if (is.null(p)) {
          err("Hiperpar\u00e1metro desconocido en ", e, ": ", nm)
          next
        }
        if (is.null(v)) next
        if (p$type %in% c("integer", "numeric")) {
          bad <- !is.numeric(v) || length(v) != 1 || is.na(v) ||
            (p$type == "integer" && v != round(v)) ||
            (!is.null(p$min) && v < p$min) || (!is.null(p$max) && v > p$max)
          if (bad) err("Valor fuera de rango para ", e, "$", nm, ": ", format(v))
        } else if (p$type == "choice" && !v %in% p$choices) {
          err("Valor no admitido para ", e, "$", nm, ": ", format(v))
        }
      }
    }
    if (!"baseline" %in% engines) {
      warn("No se incluy\u00f3 la l\u00ednea base: las m\u00e9tricas no tendr\u00e1n referencia.")
    }
  }

  if (task_ok) {
    bad_metrics <- setdiff(spec$metrics, ml_metrics_for_task(spec$task))
    if (length(spec$metrics) == 0) err("Debe indicarse al menos una m\u00e9trica.")
    if (length(bad_metrics) > 0) {
      err("M\u00e9tricas no v\u00e1lidas para la tarea: ", paste(bad_metrics, collapse = ", "))
    }
  }

  # -- Coherencia con el dataset ----------------------------------------
  if (!is.null(data) && length(errors) == 0) {
    cols <- c(spec$target, spec$predictors)
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0) {
      err("Columnas inexistentes en el dataset: ", paste(missing_cols, collapse = ", "))
    } else {
      y <- data[[spec$target]]
      detected <- ml_detect_task(y)
      if (is.na(detected)) {
        err("La variable objetivo no sirve para modelar (constante, vac\u00eda o con demasiadas clases).")
      } else if (spec$task == "regression" && !(is.numeric(y) && !is.factor(y))) {
        err("Regresi\u00f3n requiere una variable objetivo num\u00e9rica.")
      } else if (spec$task == "binary" && detected != "binary") {
        err("Clasificaci\u00f3n binaria requiere exactamente dos clases en la variable objetivo.")
      } else if (spec$task == "multiclass" && detected == "binary") {
        err("La variable objetivo tiene solo dos clases: use clasificaci\u00f3n binaria.")
      } else if (spec$task == "multiclass" && detected == "regression") {
        warn("La variable objetivo es num\u00e9rica con muchos valores: se tratar\u00e1 como categ\u00f3rica.")
      }
      if (spec$task == "binary" && is.character(spec$positive) &&
          !spec$positive %in% as.character(unique(y))) {
        err("La clase positiva '", spec$positive, "' no aparece en la variable objetivo.")
      }
      n_complete <- sum(stats::complete.cases(data[, cols, drop = FALSE]))
      if (identical(spec$na_action, "omit") && n_complete < nrow(data)) {
        warn(nrow(data) - n_complete, " filas con NA se eliminar\u00e1n (",
             format(round(100 * (nrow(data) - n_complete) / nrow(data), 1),
                    nsmall = 1, decimal.mark = ","), "%).")
      }
      n_eff <- if (identical(spec$na_action, "omit")) n_complete else nrow(data)
      if (n_eff < 2 * spec$cv$folds) {
        err("Hay muy pocas filas completas (", n_eff, ") para ", spec$cv$folds, " folds.")
      }
    }
  }

  list(valid = length(errors) == 0, errors = errors, warnings = warns)
}
