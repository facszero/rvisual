#' Ejecucion del modelado dentro del addin
#'
#' \code{ml_run()} ejecuta el MISMO script que se entrega al usuario
#' (generado por \code{ml_generate_code()}), en un entorno aislado, y
#' extrae los resultados para la interfaz. Asi las metricas que se ven en
#' pantalla son exactamente las que reproduce el script.
#'
#' Diferencias deliberadas con el script entregado, ambas fuera del
#' calculo: (1) no se ejecuta el paso de carga, porque el dataset ya esta
#' en memoria (evita releer archivos o pedir contrasenas de BD); (2) no se
#' guarda el modelo con \code{saveRDS()}.
#'
#' La ejecucion no modifica el entorno global del usuario ni su estado del
#' generador de numeros aleatorios.
#'
#' @name ml_run_model
#' @keywords internal
NULL

#' Ejecutar un modelado
#'
#' @param spec Especificacion (ver \code{ml_spec()}). Si \code{spec$final}
#'   no es \code{NULL}, tambien se entrena y evalua el modelo final.
#' @param data Data.frame con el dataset activo (antes de las operaciones
#'   del Constructor).
#' @param df_name Nombre con el que el script se refiere al dataset.
#' @param ops Operaciones del Constructor.
#' @param validate Validar la especificacion antes de ejecutar.
#' @return Lista con \code{ok}, \code{error}, \code{warnings},
#'   \code{comparison} (metricas medias por modelo), \code{cv_results}
#'   (metricas por fold), \code{final} (\code{NULL} o lista con
#'   \code{engine}, \code{fit}, \code{metrics}, \code{confusion}, \code{importance},
#'   \code{observed}, \code{predicted}), \code{n_train}, \code{n_test},
#'   \code{elapsed} (segundos), \code{output} y \code{code}.
#' @keywords internal
ml_run <- function(spec, data, df_name = "datos_rvisual", ops = list(),
                   validate = TRUE) {
  result <- list(ok = FALSE, error = NULL, warnings = character(0),
                 comparison = NULL, cv_results = NULL, final = NULL,
                 n_train = NA_integer_, n_test = NA_integer_,
                 elapsed = NA_real_, output = character(0), code = NULL)

  if (validate) {
    v <- ml_spec_validate(spec, data = ml_apply_ops_safely(data, ops))
    if (!v$valid) {
      result$error <- paste(v$errors, collapse = "\n")
      return(result)
    }
    result$warnings <- v$warnings
  }

  code <- ml_generate_code(spec, df_name, ops = ops, source = NULL,
                           timestamp = FALSE, save_model = FALSE)
  result$code <- code

  env <- new.env(parent = baseenv())
  assign(df_name, data, envir = env)
  # Segundas tablas de los joins del Constructor (ya estan en memoria)
  for (op in ops) {
    if (!identical(op$type, "join")) next
    nm <- op$params$right_df
    if (identical(nm, df_name) || exists(nm, envir = env, inherits = FALSE)) next
    right <- tryCatch(rvisual_find_dataset(nm), error = function(e) NULL)
    if (!is.null(right)) assign(nm, right, envir = env)
  }

  restore_seed <- ml_preserve_seed()
  on.exit(restore_seed(), add = TRUE)

  warns <- character(0)
  t0 <- proc.time()[["elapsed"]]
  out <- tryCatch(
    withCallingHandlers(
      utils::capture.output(eval(parse(text = code), envir = env)),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) structure(conditionMessage(e), class = "ml_run_error")
  )
  result$elapsed <- proc.time()[["elapsed"]] - t0
  result$warnings <- unique(c(result$warnings, warns))

  if (inherits(out, "ml_run_error")) {
    result$error <- paste0("Error al ejecutar el modelado: ", unclass(out))
    return(result)
  }

  result$ok <- TRUE
  result$output <- out
  result$comparison <- env$comparacion
  result$cv_results <- env$resultados
  result$n_train <- nrow(env$entrenamiento)
  result$n_test <- nrow(env$test)

  if (!is.null(spec$final) && exists("ajuste_final", envir = env, inherits = FALSE)) {
    observed  <- env$test_final[[spec$target]]
    predicted <- if (spec$task == "regression") env$pred_test else env$prob_test
    result$final <- list(
      engine     = spec$final,
      fit        = env$ajuste_final,
      metrics    = env$metricas(observed, predicted),
      confusion  = ml_confusion(spec, env, observed, predicted),
      importance = ml_importance(spec$final, env$ajuste_final, spec$predictors),
      observed   = observed,
      predicted  = predicted
    )
    # Entorno de la ejecucion: ml_run_apply() continua desde aqui
    result$env <- env
  }
  result
}

#' Aplicar el modelo final a datos nuevos dentro del addin
#'
#' Ejecuta la seccion 9 del script (ver \code{ml_code_apply()}) en el
#' entorno donde \code{ml_run()} entreno el modelo final, sin volver a
#' entrenar. Es el mismo codigo que se entrega en el script.
#'
#' @param final Resultado de \code{ml_run()} con modelo final (debe traer
#'   \code{env}).
#' @param spec Especificacion con \code{final} y \code{apply}.
#' @param df_name Nombre del dataset del entrenamiento.
#' @param ops Operaciones del Constructor.
#' @return Lista con \code{ok}, \code{error}, \code{data} (dataset
#'   resultado), \code{columns} (columnas agregadas), \code{n},
#'   \code{n_pred}, \code{unseen} (conteo de categorias no vistas por
#'   variable), \code{elapsed} y \code{code}.
#' @keywords internal
ml_run_apply <- function(final, spec, df_name, ops = list()) {
  out <- list(ok = FALSE, error = NULL, data = NULL, columns = character(0),
              n = NA_integer_, n_pred = NA_integer_, unseen = integer(0),
              elapsed = NA_real_, code = NULL)
  env <- final$env
  if (is.null(env) || is.null(spec$apply) || is.null(spec$final)) {
    out$error <- "Primero hay que entrenar el modelo final (paso 4)."
    return(out)
  }
  ap <- spec$apply
  code <- ml_code_apply(spec, df_name, ops, source = NULL)
  out$code <- code

  # Datos en memoria: el dataset destino y las segundas tablas de los joins
  nombres <- c(ap$dataset, unlist(lapply(ops, function(op)
    if (identical(op$type, "join")) op$params$right_df)))
  for (nm in setdiff(unique(nombres), ls(env))) {
    df <- tryCatch(rvisual_find_dataset(nm), error = function(e) NULL)
    if (!is.null(df)) assign(nm, df, envir = env)
  }

  restore_seed <- ml_preserve_seed()
  on.exit(restore_seed(), add = TRUE)
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch({
    suppressWarnings(utils::capture.output(eval(parse(text = code), envir = env)))
    TRUE
  }, error = function(e) conditionMessage(e))
  out$elapsed <- proc.time()[["elapsed"]] - t0
  if (!isTRUE(res)) {
    out$error <- paste0("Error al aplicar el modelo: ", res)
    return(out)
  }

  data <- get(ap$output, envir = env)
  classes <- if (spec$task == "multiclass") get("clases", envir = env) else NULL
  out$columns <- ml_apply_columns(spec, classes)
  out$data <- data
  out$n <- nrow(data)
  out$n_pred <- sum(env$completos)
  ref <- env$entrenamiento_final
  out$unseen <- vapply(spec$predictors, function(v) {
    if (!is.factor(ref[[v]])) return(0L)
    x <- as.character(env$nuevos[[v]])
    sum(!is.na(x) & !x %in% levels(ref[[v]]))
  }, integer(1))
  out$ok <- TRUE
  out
}

#' Aplicar operaciones del Constructor sin propagar errores
#' @param data Data.frame.
#' @param ops Operaciones.
#' @return El data.frame transformado, o el original si falla.
#' @keywords internal
ml_apply_ops_safely <- function(data, ops) {
  if (length(ops) == 0) return(data)
  tryCatch(apply_operations(data, ops), error = function(e) data)
}

#' Preservar el estado del generador aleatorio del usuario
#' @return Funcion que restaura el estado previo.
#' @keywords internal
ml_preserve_seed <- function() {
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = genv) else NULL
  function() {
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  }
}

#' Matriz de confusion del modelo final
#' @param spec Especificacion.
#' @param env Entorno donde se ejecuto el script.
#' @param observed,predicted Valores observados y predichos en test.
#' @return Tabla de contingencia, o \code{NULL} en regresion.
#' @keywords internal
ml_confusion <- function(spec, env, observed, predicted) {
  switch(spec$task,
    regression = NULL,
    binary = table(
      observado = observed,
      predicho  = factor(ifelse(predicted >= 0.5, env$positiva, env$negativa),
                         levels = levels(observed))
    ),
    multiclass = table(observado = observed, predicho = env$clase_predicha(predicted))
  )
}

#' Importancia de variables del modelo final
#'
#' Usa la medida nativa de cada motor: reduccion de impureza (rpart,
#' ranger), valor absoluto del estadistico t/z (lm, glm), valor absoluto
#' del coeficiente en \code{lambda.min} sobre la matriz de disenio (glmnet;
#' comparable solo si las variables estan en escalas similares) y ganancia
#' (xgboost). Los terminos de la matriz de disenio se agregan por variable
#' original sumando.
#'
#' @param engine Motor.
#' @param fit Ajuste.
#' @param predictors Predictores de la especificacion.
#' @return Data.frame \code{variable}, \code{importance} (escalada a 100 el
#'   maximo), ordenado de mayor a menor; \code{NULL} si no aplica.
#' @keywords internal
ml_importance <- function(engine, fit, predictors) {
  raw <- tryCatch(switch(engine,
    rpart  = fit$variable.importance,
    ranger = fit$variable.importance,
    lm = , glm = {
      cf <- stats::coef(summary(fit))
      cf <- cf[rownames(cf) != "(Intercept)", , drop = FALSE]
      stats::setNames(abs(cf[, 3]), rownames(cf))
    },
    glmnet = {
      cf <- stats::coef(fit, s = "lambda.min")
      if (is.list(cf)) cf <- Reduce(`+`, lapply(cf, function(m) abs(as.matrix(m))))
      cf <- as.matrix(cf)
      cf <- cf[rownames(cf) != "(Intercept)", 1]
      abs(cf)
    },
    xgboost = {
      imp <- xgboost::xgb.importance(model = fit)
      stats::setNames(imp$Gain, imp$Feature)
    },
    NULL
  ), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(NULL)

  # Agregar terminos de la matriz de disenio (ej. "sexoM") a su variable
  term_to_var <- vapply(names(raw), function(term) {
    if (term %in% predictors) return(term)
    hits <- predictors[startsWith(term, predictors)]
    if (length(hits) == 0) term else hits[which.max(nchar(hits))]
  }, character(1))
  agg <- tapply(as.numeric(raw), term_to_var, sum)
  agg <- agg[is.finite(agg)]
  if (length(agg) == 0 || max(agg) <= 0) return(NULL)
  out <- data.frame(variable = names(agg),
                    importance = 100 * as.numeric(agg) / max(agg),
                    stringsAsFactors = FALSE)
  out <- out[order(-out$importance), ]
  rownames(out) <- NULL
  out
}

#' Avisos previos a la ejecucion
#'
#' @param data Data.frame a modelar.
#' @param spec Especificacion.
#' @param max_rows Filas a partir de las cuales se recomienda ejecutar en
#'   la consola.
#' @return Character vector de avisos (puede ser vacio).
#' @keywords internal
ml_run_notices <- function(data, spec, max_rows = 100000L) {
  notices <- character(0)
  if (nrow(data) > max_rows) {
    notices <- c(notices, paste0(
      "El dataset tiene ", format(nrow(data), big.mark = ".", decimal.mark = ","),
      " filas: la ejecuci\u00f3n puede tardar y bloquear la interfaz. ",
      "Considere usar \"Ejecutar en consola\"."))
  }
  fits <- length(spec$models) * (spec$cv$folds + if (is.null(spec$final)) 0 else 1)
  if (fits > 60) {
    notices <- c(notices, paste0(
      "Se entrenar\u00e1n ", fits, " modelos (", length(spec$models),
      " candidatos \u00d7 ", spec$cv$folds, " folds)."))
  }
  notices
}
