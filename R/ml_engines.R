#' Catalogo de motores de Machine Learning
#'
#' Define los modelos disponibles en el panel Modelado: paquete que los
#' implementa, tareas que admiten e hiperparametros expuestos con sus
#' valores por defecto. El catalogo es la unica fuente de verdad para la
#' interfaz, la validacion de \code{ml_spec} y la generacion de codigo.
#'
#' Los paquetes de ML se declaran en Suggests (no en Imports): su
#' presencia se verifica en tiempo de ejecucion con
#' \code{ml_engine_available()}.
#'
#' @name ml_engines
#' @keywords internal
NULL

#' Tareas de aprendizaje supervisado soportadas
#' @return Character vector con los identificadores de tarea.
#' @keywords internal
ml_tasks <- function() {
  c("regression", "binary", "multiclass")
}

#' Definir un hiperparametro expuesto en la interfaz
#' @param default Valor por defecto. \code{NULL} significa "lo decide el paquete".
#' @param type Tipo: "integer", "numeric" o "choice".
#' @param min,max Limites admitidos (solo tipos numericos).
#' @param choices Valores admitidos (solo tipo "choice").
#' @param label Etiqueta legible para la interfaz.
#' @return Lista con la definicion del hiperparametro.
#' @keywords internal
ml_param <- function(default, type, min = NULL, max = NULL,
                     choices = NULL, label = NULL) {
  list(default = default, type = type, min = min, max = max,
       choices = choices, label = label)
}

#' Catalogo completo de motores
#'
#' @return Lista nombrada; cada elemento tiene \code{id}, \code{label},
#'   \code{package} (\code{NULL} si solo usa base R), \code{tasks} y
#'   \code{params} (lista de \code{ml_param}).
#' @examples
#' cat_ml <- rvisual:::ml_engine_catalog()
#' names(cat_ml)
#' @keywords internal
ml_engine_catalog <- function() {
  list(
    baseline = list(
      id = "baseline",
      label = "L\u00ednea base (media / clase mayoritaria)",
      package = NULL,
      tasks = ml_tasks(),
      params = list()
    ),
    lm = list(
      id = "lm",
      label = "Regresi\u00f3n lineal",
      package = NULL,
      tasks = "regression",
      params = list()
    ),
    glm = list(
      id = "glm",
      label = "Regresi\u00f3n log\u00edstica",
      package = NULL,
      tasks = "binary",
      params = list()
    ),
    glmnet = list(
      id = "glmnet",
      label = "Elastic net (glmnet)",
      package = "glmnet",
      tasks = ml_tasks(),
      params = list(
        alpha = ml_param(0.5, "numeric", min = 0, max = 1,
                         label = "alpha (0 = ridge, 1 = lasso)")
      )
    ),
    rpart = list(
      id = "rpart",
      label = "\u00c1rbol de decisi\u00f3n (rpart)",
      package = "rpart",
      tasks = ml_tasks(),
      params = list(
        cp        = ml_param(0.01, "numeric", min = 0, max = 1,
                             label = "cp (complejidad)"),
        maxdepth  = ml_param(10L, "integer", min = 1L, max = 30L,
                             label = "Profundidad m\u00e1xima"),
        minbucket = ml_param(7L, "integer", min = 1L, max = NULL,
                             label = "M\u00ednimo de casos por hoja")
      )
    ),
    ranger = list(
      id = "ranger",
      label = "Random Forest (ranger)",
      package = "ranger",
      tasks = ml_tasks(),
      params = list(
        num.trees     = ml_param(500L, "integer", min = 1L, max = NULL,
                                 label = "N\u00famero de \u00e1rboles"),
        mtry          = ml_param(NULL, "integer", min = 1L, max = NULL,
                                 label = "Variables por divisi\u00f3n (vac\u00edo = autom\u00e1tico)"),
        min.node.size = ml_param(5L, "integer", min = 1L, max = NULL,
                                 label = "Tama\u00f1o m\u00ednimo de nodo")
      )
    ),
    xgboost = list(
      id = "xgboost",
      label = "Gradient boosting (xgboost)",
      package = "xgboost",
      tasks = ml_tasks(),
      params = list(
        nrounds   = ml_param(200L, "integer", min = 1L, max = NULL,
                             label = "Iteraciones"),
        eta       = ml_param(0.1, "numeric", min = 0, max = 1,
                             label = "Tasa de aprendizaje (eta)"),
        max_depth = ml_param(6L, "integer", min = 1L, max = 20L,
                             label = "Profundidad m\u00e1xima")
      )
    )
  )
}

#' Obtener la definicion de un motor
#' @param id Identificador del motor (ej. "ranger").
#' @return Lista con la definicion del motor. Error si no existe.
#' @keywords internal
ml_engine_get <- function(id) {
  catalog <- ml_engine_catalog()
  if (!is.character(id) || length(id) != 1 || !id %in% names(catalog)) {
    stop("Motor desconocido: ", paste(id, collapse = ", "), call. = FALSE)
  }
  catalog[[id]]
}

#' Motores que admiten una tarea
#' @param task Una de \code{ml_tasks()}.
#' @return Character vector de identificadores, en orden de catalogo.
#' @keywords internal
ml_engines_for_task <- function(task) {
  task <- match.arg(task, ml_tasks())
  catalog <- ml_engine_catalog()
  names(Filter(function(e) task %in% e$tasks, catalog))
}

#' Verificar si el paquete de un motor esta instalado
#' @param id Identificador del motor.
#' @return \code{TRUE} si el motor no requiere paquetes o si el paquete
#'   esta instalado.
#' @keywords internal
ml_engine_available <- function(id) {
  pkg <- ml_engine_get(id)$package
  is.null(pkg) || requireNamespace(pkg, quietly = TRUE)
}

#' Paquetes necesarios para un conjunto de motores
#' @param ids Character vector de identificadores de motor.
#' @param task Tarea; en clasificacion binaria se agrega \code{pROC} (AUC).
#' @return Character vector unico de paquetes (puede ser vacio).
#' @keywords internal
ml_required_packages <- function(ids, task = NULL) {
  pkgs <- unlist(lapply(ids, function(id) ml_engine_get(id)$package))
  if (identical(task, "binary")) pkgs <- c(pkgs, "pROC")
  unique(as.character(pkgs))
}

#' Hiperparametros por defecto de un motor
#' @param id Identificador del motor.
#' @return Lista nombrada con los valores por defecto (los \code{NULL} se
#'   conservan como elementos \code{NULL}).
#' @keywords internal
ml_engine_defaults <- function(id) {
  params <- ml_engine_get(id)$params
  out <- vector("list", length(params))
  names(out) <- names(params)
  for (nm in names(params)) out[nm] <- list(params[[nm]]$default)
  out
}

#' Metricas disponibles por tarea
#' @param task Una de \code{ml_tasks()}.
#' @return Character vector de metricas; la primera es la de orden por defecto.
#' @keywords internal
ml_metrics_for_task <- function(task) {
  task <- match.arg(task, ml_tasks())
  switch(task,
    regression = c("rmse", "mae", "r2"),
    binary     = c("auc", "f1_macro", "accuracy", "sensitivity", "specificity"),
    multiclass = c("f1_macro", "accuracy")
  )
}
