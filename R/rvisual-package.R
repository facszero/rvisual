#' rvisual: Visual Data Analysis 'RStudio' 'Addin' with AI Copilot
#'
#' Punto central de importFrom a nivel paquete. No define funciones
#' propias; existe para declarar dependencias usadas via NSE (rlang)
#' o via base/stats/utils sin prefijo explicito en el codigo interno,
#' evitando el NOTE de R CMD check "no visible global function
#' definition".
#'
#' @keywords internal
#' @importFrom stats na.omit
#' @importFrom utils head read.csv write.csv
#' @importFrom rlang .data :=
"_PACKAGE"
