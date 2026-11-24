#' Generador de codigo del panel Modelado
#'
#' Convierte una \code{ml_spec} en un script R autocontenido: carga de
#' datos, operaciones del Constructor, preparacion, particion, folds,
#' metricas, modelos candidatos, comparacion por validacion cruzada y
#' (opcionalmente) el modelo final.
#'
#' Reutiliza \code{generate_load_step()} y \code{op_to_code_line()} del
#' generador del Constructor sin modificarlos. Todo el codigo generado es
#' ASCII y usa llamadas \code{paquete::funcion}.
#'
#' Contrato de cada modelo en el script: \code{ajustar(d)} devuelve el
#' ajuste; \code{predecir(a, nuevos)} devuelve un vector numerico
#' (regresion), la probabilidad de la clase positiva (binaria) o una
#' matriz de probabilidades con una columna por clase (multiclase).
#'
#' @name ml_codegen
#' @keywords internal
NULL

#' Generar el script R completo de un modelado
#'
#' @param spec Especificacion valida (ver \code{ml_spec()}).
#' @param df_name Nombre del objeto con el dataset activo.
#' @param ops Operaciones del Constructor (puede ser lista vacia).
#' @param source Metadatos de origen del dataset (ver
#'   \code{generate_load_step()}); \code{NULL} si se desconoce.
#' @param timestamp Incluir fecha de generacion en la cabecera. Se
#'   desactiva en tests para obtener salidas deterministas.
#' @param save_model Incluir el \code{saveRDS()} del modelo final. La
#'   ejecucion dentro del addin (\code{ml_run()}) lo desactiva para no
#'   escribir archivos.
#' @return String con el script.
#' @keywords internal
ml_generate_code <- function(spec, df_name, ops = list(), source = NULL,
                             timestamp = TRUE, save_model = TRUE) {
  engines <- vapply(spec$models, function(m) m$engine, character(1))
  sections <- c(
    ml_code_header(spec, df_name, timestamp),
    ml_code_packages(engines, spec$task),
    ml_code_data(df_name, ops, source),
    ml_code_prepare(spec),
    ml_code_split(spec),
    ml_code_folds(spec),
    ml_code_metrics(spec$task),
    ml_code_models(spec),
    ml_code_compare(spec),
    ml_code_final(spec, save_model),
    ml_code_apply(spec, df_name, ops, source)
  )
  paste(sections[nzchar(sections)], collapse = "\n\n")
}

# -- Utilidades -----------------------------------------------------------

#' Formatear un valor R como codigo
#' @param x Valor atomico de longitud 1, o vector character.
#' @return String con el literal R.
#' @keywords internal
ml_lit <- function(x) {
  if (is.character(x)) {
    q <- unname(vapply(x, function(s) encodeString(s, quote = "\""), character(1)))
    if (length(q) == 1) return(q)
    one_line <- paste0("c(", paste(q, collapse = ", "), ")")
    if (nchar(one_line) <= 70) return(one_line)
    lines <- character(0)
    current <- ""
    for (item in q) {
      candidate <- if (nzchar(current)) paste0(current, ", ", item) else item
      if (nchar(candidate) > 60 && nzchar(current)) {
        lines <- c(lines, current)
        current <- item
      } else {
        current <- candidate
      }
    }
    lines <- c(lines, current)
    return(paste0("c(\n  ", paste(lines, collapse = ",\n  "), "\n)"))
  }
  if (is.logical(x)) return(if (isTRUE(x)) "TRUE" else "FALSE")
  if (is.numeric(x) && x == round(x) && abs(x) < 1e9) {
    return(paste0(format(x, scientific = FALSE), if (is.integer(x)) "L" else ""))
  }
  format(x, digits = 15)
}

#' Renderizar hiperparametros como argumentos "nombre = valor"
#' @param params Lista nombrada; los \code{NULL} se omiten.
#' @param rename Vector nombrado opcional para traducir nombres.
#' @return String (puede ser vacio).
#' @keywords internal
ml_args <- function(params, rename = character(0)) {
  params <- Filter(Negate(is.null), params)
  if (length(params) == 0) return("")
  nms <- names(params)
  nms <- ifelse(nms %in% names(rename), rename[nms], nms)
  paste(paste0(nms, " = ", vapply(params, ml_lit, character(1))), collapse = ", ")
}

#' Cabecera de seccion en el script
#' @param title Titulo de la seccion.
#' @return String con la linea de comentario.
#' @keywords internal
ml_section <- function(title) {
  line <- paste0("# -- ", title, " ")
  paste0(line, strrep("-", max(3, 62 - nchar(line))))
}

ml_task_label <- function(task) {
  switch(task,
    regression = "regresion",
    binary     = "clasificacion binaria",
    multiclass = "clasificacion multiclase"
  )
}

# -- Secciones -----------------------------------------------------------

ml_code_header <- function(spec, df_name, timestamp) {
  paste0(
    "# ============================================================\n",
    "# Script generado por RVisual - Modelado (", ml_task_label(spec$task), ")\n",
    "# Dataset: ", df_name, " | Objetivo: ", spec$target, "\n",
    if (timestamp) paste0("# Generado: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n") else "",
    "# ============================================================"
  )
}

ml_code_packages <- function(engines, task) {
  pkgs <- ml_required_packages(engines, task)
  if (length(pkgs) == 0) return("")
  paste0(
    ml_section("Paquetes requeridos"), "\n",
    "for (p in ", ml_lit(as.character(pkgs)), ") {\n",
    "  if (!requireNamespace(p, quietly = TRUE)) stop(\"Instale el paquete: \", p)\n",
    "}"
  )
}

ml_code_data <- function(df_name, ops, source) {
  load_step <- generate_load_step(df_name, source)
  body <- if (length(ops) == 0) {
    paste0("datos <- ", df_name)
  } else {
    lines <- vapply(ops, op_to_code_line, character(1))
    paste0("datos <- ", paste(c(df_name, lines), collapse = " |>\n  "))
  }
  # Segundos datasets de los joins: solo cuando el script lleva paso de
  # carga (ml_run() lo genera sin carga y pone los datos en memoria)
  join_steps <- if (is.null(source)) character(0)
                else generate_join_load_steps(ops, df_name)
  paste(c(ml_section("1. Datos (carga + operaciones del Constructor)"),
          load_step, join_steps, body), collapse = "\n")
}

ml_code_prepare <- function(spec) {
  out <- c(
    ml_section("2. Preparacion"),
    paste0("objetivo    <- ", ml_lit(spec$target)),
    paste0("predictores <- ", ml_lit(spec$predictors))
  )
  if (spec$task == "binary") out <- c(out, paste0("positiva    <- ", ml_lit(spec$positive)))
  out <- c(out, "", "datos_ml <- as.data.frame(datos)[, c(objetivo, predictores)]")
  out <- c(out, if (identical(spec$na_action, "omit")) {
    "datos_ml <- stats::na.omit(datos_ml)"
  } else {
    c("datos_ml <- datos_ml[!is.na(datos_ml[[objetivo]]), ]",
      "# Los NA de los predictores se imputan dentro de cada entrenamiento (ver imputar_simple)")
  })
  out <- c(out,
    "# Texto y logicos a factor, con niveles fijados sobre todos los datos",
    "datos_ml[predictores] <- lapply(datos_ml[predictores], function(x) {",
    "  if (is.character(x) || is.logical(x)) factor(x) else x",
    "})"
  )
  out <- c(out, switch(spec$task,
    regression = "datos_ml[[objetivo]] <- as.numeric(datos_ml[[objetivo]])",
    binary = c(
      "negativa <- setdiff(unique(as.character(datos_ml[[objetivo]])), positiva)",
      "datos_ml[[objetivo]] <- factor(datos_ml[[objetivo]], levels = c(negativa, positiva))"
    ),
    multiclass = c(
      "datos_ml[[objetivo]] <- droplevels(factor(datos_ml[[objetivo]]))",
      "clases <- levels(datos_ml[[objetivo]])"
    )
  ))
  out <- c(out, "formula_ml <- stats::reformulate(predictores, response = objetivo)")
  engines <- vapply(spec$models, function(m) m$engine, character(1))
  if (any(engines %in% c("glmnet", "xgboost"))) {
    out <- c(out,
      "# Matriz de disenio (variables indicadoras) para glmnet / xgboost",
      "matriz_x <- function(d) stats::model.matrix(formula_ml, d)[, -1, drop = FALSE]"
    )
  }
  if (identical(spec$na_action, "impute_simple")) {
    out <- c(out, "",
      "# Imputacion simple: mediana (numericas) o moda (factores) de 'ref'",
      "imputar_simple <- function(d, ref) {",
      "  for (v in predictores) {",
      "    falta <- is.na(d[[v]])",
      "    if (!any(falta)) next",
      "    x <- ref[[v]][!is.na(ref[[v]])]",
      "    d[[v]][falta] <- if (is.numeric(x)) stats::median(x) else names(which.max(table(x)))",
      "  }",
      "  d",
      "}"
    )
  }
  paste(out, collapse = "\n")
}

ml_code_split <- function(spec) {
  strat <- isTRUE(spec$split$stratify) && spec$task != "regression"
  tp <- ml_lit(spec$split$test_prop)
  idx <- if (strat) c(
    "idx_test <- unlist(lapply(",
    "  split(seq_len(nrow(datos_ml)), datos_ml[[objetivo]]),",
    paste0("  function(i) i[sample.int(length(i), round(length(i) * ", tp, "))]"),
    "))"
  ) else paste0(
    "idx_test <- sample.int(nrow(datos_ml), round(nrow(datos_ml) * ", tp, "))"
  )
  paste(c(
    ml_section(paste0("3. Particion entrenamiento / test",
                      if (strat) " (estratificada)" else "")),
    paste0("set.seed(", ml_lit(as.integer(spec$split$seed)), ")"),
    idx,
    "entrenamiento <- datos_ml[-idx_test, ]",
    "test          <- datos_ml[idx_test, ]"
  ), collapse = "\n")
}

ml_code_folds <- function(spec) {
  strat <- spec$task != "regression"
  body <- if (strat) c(
    "folds <- integer(nrow(entrenamiento))",
    "for (i in split(seq_len(nrow(entrenamiento)), entrenamiento[[objetivo]])) {",
    "  folds[i] <- sample(rep_len(seq_len(k), length(i)))",
    "}"
  ) else "folds <- sample(rep_len(seq_len(k), nrow(entrenamiento)))"
  paste(c(
    ml_section(paste0("4. Folds de validacion cruzada",
                      if (strat) " (estratificados)" else "")),
    paste0("k <- ", ml_lit(as.integer(spec$cv$folds))),
    body
  ), collapse = "\n")
}

ml_code_metrics <- function(task) {
  body <- switch(task,
    regression = c(
      "metricas <- function(obs, pred) {",
      "  data.frame(",
      "    rmse = sqrt(mean((obs - pred)^2)),",
      "    mae  = mean(abs(obs - pred)),",
      "    r2   = 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)",
      "  )",
      "}"
    ),
    binary = c(
      "metricas <- function(obs, prob, umbral = 0.5) {",
      "  pred <- factor(ifelse(prob >= umbral, positiva, negativa), levels = levels(obs))",
      "  vp <- sum(pred == positiva & obs == positiva)",
      "  vn <- sum(pred == negativa & obs == negativa)",
      "  fp <- sum(pred == positiva & obs == negativa)",
      "  fn <- sum(pred == negativa & obs == positiva)",
      "  f1 <- function(tp, fp, fn) if (tp == 0) 0 else 2 * tp / (2 * tp + fp + fn)",
      "  data.frame(",
      "    auc           = as.numeric(pROC::auc(obs, prob, levels = c(negativa, positiva),",
      "                                         direction = \"<\", quiet = TRUE)),",
      "    f1_macro      = (f1(vp, fp, fn) + f1(vn, fn, fp)) / 2,",
      "    exactitud     = (vp + vn) / length(obs),",
      "    sensibilidad  = vp / (vp + fn),",
      "    especificidad = vn / (vn + fp)",
      "  )",
      "}"
    ),
    multiclass = c(
      "clase_predicha <- function(prob) {",
      "  factor(colnames(prob)[max.col(prob, ties.method = \"first\")], levels = clases)",
      "}",
      "metricas <- function(obs, prob) {",
      "  pred <- clase_predicha(prob)",
      "  f1 <- vapply(clases, function(cl) {",
      "    tp <- sum(pred == cl & obs == cl)",
      "    if (tp == 0) return(0)",
      "    2 * tp / (2 * tp + sum(pred == cl & obs != cl) + sum(pred != cl & obs == cl))",
      "  }, numeric(1))",
      "  data.frame(",
      "    f1_macro  = mean(f1),",
      "    exactitud = mean(pred == obs)",
      "  )",
      "}"
    )
  )
  paste(c(ml_section("5. Metricas"), body), collapse = "\n")
}

#' Nombres de columna de metricas en el script generado
#' @param ids Identificadores de metrica (ver \code{ml_metrics_for_task()}).
#' @return Character vector con los nombres usados en el script.
#' @keywords internal
ml_metric_columns <- function(ids) {
  map <- c(rmse = "rmse", mae = "mae", r2 = "r2", auc = "auc",
           f1_macro = "f1_macro", accuracy = "exactitud",
           sensitivity = "sensibilidad", specificity = "especificidad")
  unname(map[ids])
}

ml_code_models <- function(spec) {
  contrato <- switch(spec$task,
    regression = "valor predicho",
    binary     = "P(positiva)",
    multiclass = "matriz de probabilidades (una columna por clase)"
  )
  blocks <- vapply(spec$models, function(m) {
    ml_engine_template(m$engine, m$params, spec$task, spec$split$seed)
  }, character(1))
  paste0(
    ml_section("6. Modelos candidatos"), "\n",
    "# ajustar(d) -> ajuste ; predecir(a, nuevos) -> ", contrato, "\n",
    "modelos <- list(\n", paste(blocks, collapse = ",\n"), "\n)"
  )
}

#' Plantilla de codigo de un motor
#' @param engine Identificador del motor.
#' @param params Hiperparametros (lista nombrada).
#' @param task Tarea.
#' @param seed Semilla (para motores aleatorios).
#' @return String con el elemento \code{nombre = list(ajustar, predecir)}.
#' @keywords internal
ml_engine_template <- function(engine, params, task, seed) {
  mm  <- "matriz_x(d)"
  mmn <- "matriz_x(nuevos)"
  sep <- function(a) if (nzchar(a)) paste0(", ", a) else ""

  fit_pred <- switch(engine,

    baseline = switch(task,
      regression = c("function(d) mean(d[[objetivo]])",
                     "function(a, nuevos) rep(a, nrow(nuevos))"),
      binary     = c("function(d) mean(d[[objetivo]] == positiva)",
                     "function(a, nuevos) rep(a, nrow(nuevos))"),
      multiclass = c("function(d) prop.table(table(d[[objetivo]]))[clases]",
                     paste0("function(a, nuevos) matrix(rep(as.numeric(a), each = nrow(nuevos)),\n",
                            "                                   ncol = length(clases), dimnames = list(NULL, clases))"))
    ),

    lm = c("function(d) stats::lm(formula_ml, data = d)",
           "function(a, nuevos) unname(stats::predict(a, nuevos))"),

    glm = c("function(d) stats::glm(formula_ml, data = d, family = stats::binomial())",
            "function(a, nuevos) unname(stats::predict(a, nuevos, type = \"response\"))"),

    glmnet = {
      fam <- switch(task, regression = "gaussian", binary = "binomial",
                    multiclass = "multinomial")
      fit <- paste0("function(d) glmnet::cv.glmnet(", mm, ", d[[objetivo]],\n",
                    "                                       family = \"", fam, "\"",
                    sep(ml_args(params)),
                    ",\n                                       nfolds = max(3, min(10, nrow(d) %/% 3)))")
      pred <- switch(task,
        regression = paste0("function(a, nuevos) as.numeric(stats::predict(a, ", mmn,
                            ", s = \"lambda.min\"))"),
        binary     = paste0("function(a, nuevos) as.numeric(stats::predict(a, ", mmn,
                            ",\n                                       s = \"lambda.min\", type = \"response\"))"),
        multiclass = paste0("function(a, nuevos) stats::predict(a, ", mmn,
                            ",\n                                       s = \"lambda.min\", type = \"response\")[, clases, 1]")
      )
      c(fit, pred)
    },

    rpart = {
      method <- if (task == "regression") "anova" else "class"
      fit <- paste0("function(d) rpart::rpart(formula_ml, data = d, method = \"", method, "\",\n",
                    "                                       control = rpart::rpart.control(",
                    ml_args(params), "))")
      pred <- switch(task,
        regression = "function(a, nuevos) unname(stats::predict(a, nuevos))",
        binary     = "function(a, nuevos) unname(stats::predict(a, nuevos, type = \"prob\")[, positiva])",
        multiclass = "function(a, nuevos) stats::predict(a, nuevos, type = \"prob\")[, clases, drop = FALSE]"
      )
      c(fit, pred)
    },

    ranger = {
      prob <- if (task == "regression") "" else "probability = TRUE, "
      fit <- paste0("function(d) ranger::ranger(formula_ml, data = d, ", prob,
                    ml_args(params), ",\n",
                    "                                       importance = \"impurity\", seed = ",
                    ml_lit(as.integer(seed)), ")")
      pred <- switch(task,
        regression = "function(a, nuevos) stats::predict(a, nuevos)$predictions",
        binary     = "function(a, nuevos) unname(stats::predict(a, nuevos)$predictions[, positiva])",
        multiclass = "function(a, nuevos) stats::predict(a, nuevos)$predictions[, clases, drop = FALSE]"
      )
      c(fit, pred)
    },

    xgboost = {
      obj <- switch(task, regression = "reg:squarederror", binary = "binary:logistic",
                    multiclass = "multi:softprob")
      label <- switch(task,
        regression = "d[[objetivo]]",
        binary     = "as.integer(d[[objetivo]] == positiva)",
        multiclass = "as.integer(d[[objetivo]]) - 1L"
      )
      nrounds <- params$nrounds %||% 200L
      booster <- params[setdiff(names(params), "nrounds")]
      extra <- if (task == "multiclass") ", num_class = length(clases)" else ""
      fit <- paste0(
        "function(d) {\n",
        "      set.seed(", ml_lit(as.integer(seed)), ")\n",
        "      xgboost::xgb.train(\n",
        "        params = list(objective = \"", obj, "\"", sep(ml_args(booster)), extra, "),\n",
        "        data = xgboost::xgb.DMatrix(", mm, ", label = ", label, "),\n",
        "        nrounds = ", ml_lit(as.integer(nrounds)), ", verbose = 0)\n",
        "    }")
      pred <- switch(task,
        regression = ,
        binary     = paste0("function(a, nuevos) as.numeric(stats::predict(a, xgboost::xgb.DMatrix(", mmn, ")))"),
        multiclass = paste0(
          "function(a, nuevos) {\n",
          "      p <- stats::predict(a, xgboost::xgb.DMatrix(", mmn, "))\n",
          "      if (is.null(dim(p))) p <- matrix(p, ncol = length(clases), byrow = TRUE)\n",
          "      dimnames(p) <- list(NULL, clases)\n",
          "      p\n",
          "    }")
      )
      c(fit, pred)
    },

    stop("Motor sin plantilla: ", engine, call. = FALSE)
  )

  paste0(
    "  ", engine, " = list(\n",
    "    ajustar  = ", fit_pred[1], ",\n",
    "    predecir = ", fit_pred[2], "\n",
    "  )"
  )
}

ml_code_compare <- function(spec) {
  cols <- ml_metric_columns(spec$metrics)
  first <- cols[1]
  ascending <- first %in% c("rmse", "mae")
  prep <- if (identical(spec$na_action, "impute_simple")) c(
    "    va <- imputar_simple(va, tr)",
    "    tr <- imputar_simple(tr, tr)"
  ) else character(0)
  paste(c(
    ml_section("7. Comparacion por validacion cruzada"),
    "resultados <- do.call(rbind, lapply(names(modelos), function(m) {",
    "  do.call(rbind, lapply(seq_len(k), function(f) {",
    "    tr <- entrenamiento[folds != f, ]",
    "    va <- entrenamiento[folds == f, ]",
    prep,
    "    ajuste <- modelos[[m]]$ajustar(tr)",
    "    cbind(modelo = m, fold = f, metricas(va[[objetivo]], modelos[[m]]$predecir(ajuste, va)))",
    "  }))",
    "}))",
    paste0("comparacion <- stats::aggregate(cbind(", paste(cols, collapse = ", "),
           ") ~ modelo, data = resultados, FUN = mean)"),
    paste0("comparacion <- comparacion[order(", if (ascending) "" else "-",
           "comparacion$", first, "), ]"),
    "print(comparacion, digits = 3, row.names = FALSE)"
  ), collapse = "\n")
}

ml_code_final <- function(spec, save_model = TRUE) {
  engines <- vapply(spec$models, function(m) m$engine, character(1))
  final <- spec$final
  choice <- if (is.null(final)) {
    paste0("modelo_elegido <- NULL  # elegir uno de: ",
           paste(engines[engines != "baseline"], collapse = ", "))
  } else {
    paste0("modelo_elegido <- ", ml_lit(final))
  }
  prep <- if (identical(spec$na_action, "impute_simple")) c(
    "  test_final <- imputar_simple(test, entrenamiento)",
    "  entrenamiento_final <- imputar_simple(entrenamiento, entrenamiento)"
  ) else c(
    "  test_final <- test",
    "  entrenamiento_final <- entrenamiento"
  )
  eval_lines <- switch(spec$task,
    regression = c(
      "  pred_test <- modelos[[modelo_elegido]]$predecir(ajuste_final, test_final)",
      "  print(metricas(test_final[[objetivo]], pred_test), digits = 3, row.names = FALSE)"
    ),
    binary = c(
      "  prob_test <- modelos[[modelo_elegido]]$predecir(ajuste_final, test_final)",
      "  print(metricas(test_final[[objetivo]], prob_test), digits = 3, row.names = FALSE)",
      "  print(table(observado = test_final[[objetivo]],",
      "              predicho  = ifelse(prob_test >= 0.5, positiva, negativa)))"
    ),
    multiclass = c(
      "  prob_test <- modelos[[modelo_elegido]]$predecir(ajuste_final, test_final)",
      "  print(metricas(test_final[[objetivo]], prob_test), digits = 3, row.names = FALSE)",
      "  print(table(observado = test_final[[objetivo]], predicho = clase_predicha(prob_test)))"
    )
  )
  paste(c(
    ml_section("8. Modelo final (elegido por el analista)"),
    choice,
    "if (!is.null(modelo_elegido)) {",
    prep,
    "  ajuste_final <- modelos[[modelo_elegido]]$ajustar(entrenamiento_final)",
    "  # Evaluacion en test: una sola vez, despues de elegir",
    eval_lines,
    if (save_model) "  saveRDS(ajuste_final, paste0(\"modelo_\", modelo_elegido, \".rds\"))",
    "}"
  ), collapse = "\n")
}

#' Especificacion de la aplicacion del modelo a datos nuevos
#'
#' @param dataset Nombre del dataset sobre el que se predice.
#' @param ops_idx Indices de las operaciones del Constructor que se aplican
#'   a ese dataset antes de predecir (por defecto, ninguna).
#' @param filter Operacion de filtro adicional (ver \code{op_filter()}) o
#'   \code{NULL}.
#' @param output Nombre del dataset resultado.
#' @param source Origen del dataset (ver \code{generate_load_step()}), para
#'   el paso de carga cuando no es el dataset del entrenamiento.
#' @return Lista que se guarda en \code{spec$apply}.
#' @keywords internal
ml_apply_spec <- function(dataset, ops_idx = integer(0), filter = NULL,
                          output = paste0(dataset, "_pred"), source = NULL) {
  stopifnot(is.character(dataset), length(dataset) == 1, nzchar(dataset),
            is.character(output), length(output) == 1, nzchar(output))
  list(dataset = dataset, ops_idx = as.integer(ops_idx), filter = filter,
       output = output, source = source)
}

#' Nombres de las columnas que agrega la prediccion
#' @param spec Especificacion con \code{target}, \code{task} y
#'   \code{positive}.
#' @param classes Clases (solo multiclase).
#' @return Character vector.
#' @keywords internal
ml_apply_columns <- function(spec, classes = NULL) {
  switch(spec$task,
    regression = paste0("pred_", spec$target),
    binary     = c(paste0("prob_", spec$positive), "clase_pred"),
    multiclass = c(paste0("prob_", classes), "clase_pred")
  )
}

#' Seccion 9 del script: aplicar el modelo final a datos nuevos
#'
#' Prepara los datos nuevos igual que el entrenamiento: mismos tipos, mismos
#' niveles de los factores (una categoria que el modelo no vio queda como
#' NA) e imputacion con la referencia del entrenamiento, nunca con los datos
#' nuevos. Las filas que siguen incompletas quedan sin prediccion (NA).
#'
#' @inheritParams ml_generate_code
#' @return String con la seccion, o \code{""} si \code{spec$apply} o
#'   \code{spec$final} son \code{NULL}.
#' @keywords internal
ml_code_apply <- function(spec, df_name, ops = list(), source = NULL) {
  ap <- spec$apply
  if (is.null(ap) || is.null(spec$final)) return("")

  load <- if (!is.null(source) && !identical(ap$dataset, df_name) && !is.null(ap$source))
    generate_load_step(ap$dataset, ap$source) else NULL
  sel_ops <- ops[intersect(ap$ops_idx, seq_along(ops))]
  if (!is.null(ap$filter)) sel_ops <- c(sel_ops, list(ap$filter))
  lines <- vapply(sel_ops, op_to_code_line, character(1))
  build <- paste0("nuevos <- ", paste(c(ap$dataset, lines), collapse = " |>\n  "))

  prep <- c(
    "  nuevos_ml <- nuevos[, predictores, drop = FALSE]",
    "  # Mismos tipos y niveles que en el entrenamiento (categoria nueva -> NA)",
    "  for (v in predictores) {",
    "    ref <- entrenamiento_final[[v]]",
    "    nuevos_ml[[v]] <- if (is.factor(ref)) {",
    "      factor(as.character(nuevos_ml[[v]]), levels = levels(ref))",
    "    } else {",
    "      as.numeric(nuevos_ml[[v]])",
    "    }",
    "  }",
    if (identical(spec$na_action, "impute_simple"))
      "  nuevos_ml <- imputar_simple(nuevos_ml, entrenamiento_final)",
    "  # Columna auxiliar: la formula del modelo nombra al objetivo",
    "  nuevos_ml[[objetivo]] <- rep(entrenamiento_final[[objetivo]][1], nrow(nuevos_ml))",
    "  completos <- stats::complete.cases(nuevos_ml)"
  )
  cols <- ml_apply_columns(spec, classes = "CLASE")
  pred <- switch(spec$task,
    regression = c(
      "  pred_nuevos <- rep(NA_real_, nrow(nuevos_ml))",
      "  if (any(completos)) {",
      "    pred_nuevos[completos] <- modelos[[modelo_elegido]]$predecir(",
      "      ajuste_final, nuevos_ml[completos, , drop = FALSE])",
      "  }",
      paste0("  nuevos[[", ml_lit(cols[1]), "]] <- pred_nuevos")
    ),
    binary = c(
      "  prob_nuevos <- rep(NA_real_, nrow(nuevos_ml))",
      "  if (any(completos)) {",
      "    prob_nuevos[completos] <- modelos[[modelo_elegido]]$predecir(",
      "      ajuste_final, nuevos_ml[completos, , drop = FALSE])",
      "  }",
      paste0("  nuevos[[", ml_lit(cols[1]), "]] <- prob_nuevos"),
      "  nuevos[[\"clase_pred\"]] <- ifelse(prob_nuevos >= 0.5, positiva, negativa)"
    ),
    multiclass = c(
      "  prob_nuevos <- matrix(NA_real_, nrow(nuevos_ml), length(clases),",
      "                        dimnames = list(NULL, clases))",
      "  if (any(completos)) {",
      "    prob_nuevos[completos, ] <- modelos[[modelo_elegido]]$predecir(",
      "      ajuste_final, nuevos_ml[completos, , drop = FALSE])",
      "  }",
      "  for (cl in clases) nuevos[[paste0(\"prob_\", cl)]] <- prob_nuevos[, cl]",
      "  nuevos[[\"clase_pred\"]] <- ifelse(completos, as.character(clase_predicha(prob_nuevos)), NA)"
    )
  )
  paste(c(
    ml_section("9. Aplicar el modelo a datos nuevos"),
    load,
    build,
    "nuevos <- as.data.frame(nuevos)",
    "if (!is.null(modelo_elegido)) {",
    prep,
    pred,
    paste0("  ", ap$output, " <- nuevos"),
    "  cat(\"Predicciones:\", sum(completos), \"de\", nrow(nuevos), \"filas\\n\")",
    "}"
  ), collapse = "\n")
}
