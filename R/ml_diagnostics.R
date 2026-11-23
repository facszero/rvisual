#' Diagnostico del dataset para el panel Modelado
#'
#' Conjunto fijo de reglas deterministas. Cada hallazgo informa el dato
#' que lo disparo, el umbral aplicado y la consecuencia. No hay
#' puntuaciones opacas ni recomendacion automatica de un "mejor" modelo:
#' el diagnostico solo pre-marca candidatos y explica por que.
#'
#' @name ml_diagnostics
#' @keywords internal
NULL

#' Umbrales del diagnostico
#'
#' Valores iniciales, a calibrar con datos institucionales.
#'
#' @return Lista nombrada de umbrales.
#' @keywords internal
ml_diag_thresholds <- function() {
  list(
    few_rows          = 200,   # n por debajo -> pocas filas
    rows_per_term     = 10,    # n / terminos del modelo por debajo -> muchos predictores
    minority_share    = 0.10,  # proporcion de la clase minoritaria
    min_class_per_fold = 5,    # casos por clase y por fold
    zero_share        = 0.30,  # proporcion de ceros en el objetivo (regresion)
    skewness          = 2,     # asimetria del objetivo positivo
    na_warn           = 0.05,  # NA por columna: aviso
    na_drop           = 0.40,  # NA por columna: sugerir excluir
    max_levels        = 50,    # niveles de un factor
    leak_cor          = 0.95,  # correlacion absoluta con el objetivo
    leak_min_per_level = 5     # casos medios por nivel para evaluar pureza
  )
}

#' Perfil descriptivo del dataset respecto de un objetivo
#'
#' @param data Data.frame.
#' @param target Nombre de la variable objetivo.
#' @param predictors Nombres de los predictores.
#' @return Lista con \code{n}, \code{n_complete}, \code{target_na},
#'   \code{columns} (data.frame con tipo, NA, niveles y valores unicos por
#'   predictor) y \code{target_summary}.
#' @keywords internal
ml_profile <- function(data, target, predictors) {
  data <- as.data.frame(data)
  n <- nrow(data)
  y <- data[[target]]
  columns <- data.frame(variable = character(0), type = character(0),
                        na_share = numeric(0), n_unique = integer(0),
                        stringsAsFactors = FALSE)
  if (length(predictors) > 0) columns <- do.call(rbind, lapply(predictors, function(v) {
    x <- data[[v]]
    categorical <- is.character(x) || is.factor(x) || is.logical(x)
    data.frame(
      variable = v,
      type     = if (categorical) "categorica" else if (is.numeric(x)) "numerica" else class(x)[1],
      na_share = if (n > 0) mean(is.na(x)) else 0,
      n_unique = length(unique(x[!is.na(x)])),
      stringsAsFactors = FALSE
    )
  }))
  y_ok <- y[!is.na(y)]
  target_summary <- if (is.numeric(y) && !is.factor(y) && length(unique(y_ok)) > 2) {
    list(kind = "numeric",
         mean = mean(y_ok), sd = stats::sd(y_ok),
         min = min(y_ok), max = max(y_ok),
         zero_share = mean(y_ok == 0))
  } else {
    tab <- table(as.character(y_ok))
    list(kind = "categorical",
         counts = as.integer(tab), classes = names(tab),
         shares = as.numeric(prop.table(tab)))
  }
  list(
    n = n,
    n_complete = sum(stats::complete.cases(data[, c(target, predictors), drop = FALSE])),
    target_na = sum(is.na(y)),
    columns = columns,
    target_summary = target_summary
  )
}

#' Crear un hallazgo del diagnostico
#' @param id Identificador estable de la regla.
#' @param severity "info", "warning" o "critical".
#' @param message Texto para el usuario.
#' @param evidence Dato concreto que disparo la regla.
#' @param suggest Lista con efectos sugeridos: \code{engines},
#'   \code{exclude}, \code{folds}.
#' @return Lista con los campos anteriores.
#' @keywords internal
ml_finding <- function(id, severity, message, evidence = "", suggest = list()) {
  list(id = id, severity = severity, message = message,
       evidence = evidence, suggest = suggest)
}

#' Diagnosticar un dataset para modelado
#'
#' @param data Data.frame.
#' @param target Nombre de la variable objetivo.
#' @param predictors Nombres de los predictores.
#' @param task Tarea; si es \code{NULL} se infiere con \code{ml_detect_task()}.
#' @param folds Folds de validacion cruzada previstos.
#' @param positive Clase positiva (binaria); si es \code{NULL} se toma la
#'   clase minoritaria.
#' @param thresholds Umbrales (ver \code{ml_diag_thresholds()}).
#' @return Lista con \code{task}, \code{profile}, \code{findings} (lista
#'   de \code{ml_finding}, ordenada por severidad) y \code{suggested}
#'   (\code{engines}, \code{exclude}, \code{folds}).
#' @examples
#' d <- rvisual:::ml_diagnose(mtcars, "mpg", c("wt", "hp", "cyl"))
#' vapply(d$findings, function(f) f$id, character(1))
#' @keywords internal
ml_diagnose <- function(data, target, predictors, task = NULL, folds = 5L,
                        positive = NULL, thresholds = ml_diag_thresholds()) {
  data <- as.data.frame(data)
  th <- thresholds
  y <- data[[target]]
  if (is.null(task)) task <- ml_detect_task(y)
  profile <- ml_profile(data, target, predictors)
  n <- profile$n
  cols <- profile$columns
  findings <- list()
  add <- function(f) findings[[length(findings) + 1]] <<- f
  num <- function(x, d) format(round(x, d), nsmall = d, decimal.mark = ",")
  pct <- function(x) paste0(num(100 * x, 1), "%")

  if (is.na(task)) {
    add(ml_finding("target_unusable", "critical",
      "La variable objetivo no sirve para modelar: es constante, est\u00e1 vac\u00eda o tiene demasiadas clases.",
      paste0(length(unique(y[!is.na(y)])), " valores distintos")))
    return(ml_diag_result(task, profile, findings, character(0), character(0), folds))
  }

  # -- Objetivo con NA ---------------------------------------------------
  if (profile$target_na > 0) {
    add(ml_finding("target_na", "warning",
      "Hay filas sin valor en la variable objetivo: no se usan para entrenar ni evaluar.",
      paste0(profile$target_na, " filas (", pct(profile$target_na / n), ")")))
  }

  # -- Pocas filas -------------------------------------------------------
  n_use <- n - profile$target_na
  suggested_folds <- folds
  if (n_use < th$few_rows) {
    suggested_folds <- max(folds, 10L)
    add(ml_finding("few_rows", "warning",
      "Pocas filas: conviene validaci\u00f3n cruzada con 10 folds y preferir modelos simples (lineal/log\u00edstica, \u00e1rbol).",
      paste0(n_use, " filas utilizables (umbral: ", th$few_rows, ")"),
      list(folds = 10L)))
  }

  # -- Identificadores, alta cardinalidad y NA por columna ----------------
  exclude <- character(0)
  for (i in seq_len(nrow(cols))) {
    v <- cols$variable[i]
    x <- data[[v]]
    x_ok <- x[!is.na(x)]
    if (ml_looks_like_id(x)) {
      exclude <- c(exclude, v)
      add(ml_finding("identifier", "warning",
        paste0("'", v, "' parece un identificador (un valor distinto por fila): se excluye por defecto."),
        paste0(cols$n_unique[i], " valores distintos en ", length(x_ok), " filas"),
        list(exclude = v)))
      next
    }
    if (cols$type[i] == "categorica" && cols$n_unique[i] > th$max_levels) {
      exclude <- c(exclude, v)
      add(ml_finding("high_cardinality", "warning",
        paste0("'", v, "' tiene muchas categor\u00edas: se excluye por defecto. Random Forest y boosting la toleran mejor que los modelos lineales."),
        paste0(cols$n_unique[i], " niveles (umbral: ", th$max_levels, ")"),
        list(exclude = v)))
      next
    }
    if (cols$na_share[i] > th$na_drop) {
      exclude <- c(exclude, v)
      add(ml_finding("na_massive", "warning",
        paste0("'", v, "' tiene NA en una proporci\u00f3n muy alta: se sugiere excluirla."),
        paste0(pct(cols$na_share[i]), " de NA (umbral: ", pct(th$na_drop), ")"),
        list(exclude = v)))
    } else if (cols$na_share[i] > th$na_warn) {
      add(ml_finding("na_predictor", "info",
        paste0("'", v, "' tiene NA: si se eliminan filas incompletas se pierden datos; considerar imputaci\u00f3n simple."),
        paste0(pct(cols$na_share[i]), " de NA")))
    }
  }
  kept <- setdiff(predictors, exclude)

  # -- Filas que se pierden con na.omit (sobre predictores conservados) ----
  if (length(kept) > 0) {
    n_complete <- sum(stats::complete.cases(data[, c(target, kept), drop = FALSE]))
    lost <- n_use - n_complete
    if (lost > 0 && lost / max(n_use, 1) > th$na_warn) {
      add(ml_finding("rows_lost", "warning",
        "Eliminar las filas incompletas reducir\u00eda bastante los datos: considerar imputaci\u00f3n simple.",
        paste0(lost, " filas de ", n_use, " (", pct(lost / n_use), ")")))
    }
  }

  # -- Muchos predictores --------------------------------------------------
  terms <- sum(vapply(kept, function(v) {
    x <- data[[v]]
    if (is.character(x) || is.factor(x) || is.logical(x)) {
      max(length(unique(x[!is.na(x)])) - 1L, 1L)
    } else 1L
  }, integer(1)))
  many_predictors <- terms > 0 && n_use / terms < th$rows_per_term
  if (many_predictors) {
    add(ml_finding("many_predictors", "warning",
      "Muchos t\u00e9rminos para la cantidad de filas: se pre-marca elastic net (regularizaci\u00f3n).",
      paste0(terms, " t\u00e9rminos para ", n_use, " filas (menos de ", th$rows_per_term, " filas por t\u00e9rmino)"),
      list(engines = "glmnet")))
  }

  # -- Reglas del objetivo ---------------------------------------------------
  if (task == "regression") {
    y_ok <- y[!is.na(y)]
    zero_share <- mean(y_ok == 0)
    if (zero_share > th$zero_share) {
      add(ml_finding("many_zeros", "warning",
        "La variable objetivo tiene muchos ceros: un modelo en dos etapas (\u00bfes cero? / valor si no lo es) puede convenir. Disponible en la fase 2.",
        paste0(pct(zero_share), " de ceros (umbral: ", pct(th$zero_share), ")")))
    }
    if (all(y_ok > 0) && length(y_ok) > 2) {
      sk <- mean((y_ok - mean(y_ok))^3) / stats::sd(y_ok)^3
      if (is.finite(sk) && sk > th$skewness) {
        add(ml_finding("skewed_target", "info",
          "La variable objetivo es muy asim\u00e9trica: comparar tambi\u00e9n modelando log(objetivo).",
          paste0("asimetr\u00eda ", num(sk, 2), " (umbral: ", th$skewness, ")")))
      }
    }
  } else {
    counts <- table(as.character(y[!is.na(y)]))
    shares <- prop.table(counts)
    minority <- names(which.min(counts))
    if (min(shares) < th$minority_share) {
      add(ml_finding("imbalanced", "warning",
        "Clases desbalanceadas: la exactitud enga\u00f1a; mirar F1-macro y AUC. La partici\u00f3n se estratifica.",
        paste0("clase '", minority, "': ", pct(min(shares)), " (umbral: ", pct(th$minority_share), ")")))
    }
    small <- names(counts)[counts < th$min_class_per_fold * suggested_folds]
    if (length(small) > 0) {
      add(ml_finding("small_class", "critical",
        "Alguna clase tiene muy pocos casos: la validaci\u00f3n cruzada puede dejar folds sin ella.",
        paste0(paste0("'", small, "' (", counts[small], ")", collapse = ", "),
               "; m\u00ednimo recomendado con ", suggested_folds, " folds: ",
               th$min_class_per_fold * suggested_folds)))
    }
  }

  # -- Posible fuga de informacion ------------------------------------------
  y_num <- if (task == "regression") y else if (task == "binary") {
    pos <- positive %||% names(which.min(table(as.character(y[!is.na(y)]))))
    as.numeric(as.character(y) == pos)
  } else NULL
  for (v in kept) {
    x <- data[[v]]
    leak <- NULL
    if (is.numeric(x) && !is.null(y_num)) {
      ok <- !is.na(x) & !is.na(y_num)
      if (sum(ok) > 2 && stats::sd(x[ok]) > 0 && stats::sd(y_num[ok]) > 0) {
        r <- stats::cor(x[ok], y_num[ok])
        if (abs(r) > th$leak_cor) leak <- paste0("correlaci\u00f3n ", num(r, 3))
      }
    } else if (!is.numeric(x) && task != "regression") {
      ok <- !is.na(x) & !is.na(y)
      levels_x <- unique(as.character(x[ok]))
      if (length(levels_x) >= 2 && sum(ok) / length(levels_x) >= th$leak_min_per_level) {
        pure <- all(tapply(as.character(y[ok]), as.character(x[ok]),
                           function(z) length(unique(z)) == 1))
        if (pure) leak <- "cada categor\u00eda corresponde a una sola clase"
      }
    }
    if (!is.null(leak)) {
      add(ml_finding("leakage", "critical",
        paste0("'", v, "' predice el objetivo casi perfectamente: verificar que ese dato exista en el momento de predecir (posible fuga de informaci\u00f3n)."),
        leak))
    }
  }

  # -- Siempre: comparar capacidades distintas -----------------------------
  add(ml_finding("nonlinear", "info",
    "Se incluyen al menos un \u00e1rbol y un ensamble junto al modelo lineal: la elecci\u00f3n se decide comparando contra la l\u00ednea base, no por una regla previa.",
    "", list(engines = c("rpart", "ranger"))))

  linear <- switch(task, regression = "lm", binary = "glm", multiclass = character(0))
  engines <- c("baseline", linear, if (many_predictors) "glmnet", "rpart", "ranger")
  ml_diag_result(task, profile, findings, engines, unique(exclude), suggested_folds)
}

ml_diag_result <- function(task, profile, findings, engines, exclude, folds) {
  order_sev <- c(critical = 1, warning = 2, info = 3)
  if (length(findings) > 0) {
    sev <- vapply(findings, function(f) order_sev[[f$severity]], numeric(1))
    findings <- findings[order(sev)]
  }
  engines <- Filter(ml_engine_available, engines)
  if (!is.na(task)) engines <- intersect(engines, ml_engines_for_task(task))
  list(task = task, profile = profile, findings = findings,
       suggested = list(engines = unique(as.character(engines)),
                        exclude = exclude, folds = as.integer(folds)))
}

#' Detectar columnas con aspecto de identificador
#'
#' Texto o factor con un valor distinto por fila, o enteros consecutivos
#' (1..n en algun orden).
#'
#' @param x Vector.
#' @return Logico.
#' @keywords internal
ml_looks_like_id <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 10 || length(unique(x)) != n) return(FALSE)
  if (is.character(x) || is.factor(x)) return(TRUE)
  if (is.numeric(x) && all(x == round(x))) return(max(x) - min(x) == n - 1)
  FALSE
}

#' Pasar el resultado del diagnostico a una ml_spec
#'
#' Aplica las sugerencias: excluye predictores, fija los folds y
#' selecciona los modelos sugeridos (con hiperparametros por defecto).
#'
#' @param diagnosis Resultado de \code{ml_diagnose()}.
#' @param target Variable objetivo.
#' @param predictors Predictores elegidos por el usuario.
#' @param positive Clase positiva (binaria).
#' @return Una \code{ml_spec}.
#' @keywords internal
ml_spec_from_diagnosis <- function(diagnosis, target, predictors, positive = NULL) {
  ml_spec(
    target     = target,
    predictors = setdiff(predictors, diagnosis$suggested$exclude),
    task       = diagnosis$task,
    positive   = positive,
    models     = lapply(diagnosis$suggested$engines, ml_model),
    folds      = diagnosis$suggested$folds
  )
}
