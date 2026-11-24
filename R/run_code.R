#' Ejecutar codigo generado dentro del addin
#'
#' El codigo que genera RVisual es autocontenido: carga sus datos al
#' principio. Pero algunos datasets solo existen dentro de RVisual (por
#' ejemplo, las predicciones del panel Modelado) y su "paso de carga" es un
#' comentario. Esta funcion ejecuta el codigo en el entorno global, como
#' siempre; si el codigo usa un dataset que no esta en el entorno global ni
#' lo crea el propio codigo, pero si esta cargado en RVisual, lo ejecuta en
#' un entorno hijo del global que contiene esa copia. Asi el codigo
#' funciona igual que en la vista previa, sin escribir en el entorno global
#' datasets que el usuario no pidio.
#'
#' @param code String con el codigo R.
#' @return Lista con \code{value} (valor de la ultima expresion) y
#'   \code{from_rvisual} (nombres de los datasets tomados de RVisual).
#' @keywords internal
rvisual_eval_code <- function(code) {
  exprs <- parse(text = code)
  faltan <- rvisual_code_needs(exprs)
  env <- globalenv()
  if (length(faltan) > 0) {
    env <- new.env(parent = globalenv())
    for (nm in faltan) assign(nm, get(nm, envir = rvisual_env()), envir = env)
  }
  value <- NULL
  for (e in exprs) value <- eval(e, envir = env)
  list(value = value, from_rvisual = faltan)
}

#' Datasets de RVisual que necesita un codigo
#' @param exprs Expresiones (resultado de \code{parse()}).
#' @param global Entorno global de R (parametro para los tests).
#' @return Nombres de datasets cargados en RVisual que el codigo usa, que
#'   no crea con una asignacion de primer nivel y que no estan en el
#'   entorno global.
#' @keywords internal
rvisual_code_needs <- function(exprs, global = globalenv()) {
  usados <- unique(unlist(lapply(exprs, all.names)))
  # Nombres que el propio codigo crea con una asignacion de primer nivel
  creados <- unlist(lapply(exprs, function(e) {
    if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]))
      as.character(e[[2]])
  }))
  propios <- ls(envir = rvisual_env())
  faltan <- setdiff(intersect(usados, propios), creados)
  faltan[!vapply(faltan, exists, logical(1), envir = global, inherits = FALSE)]
}
