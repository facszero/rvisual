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
