#' Claves de API en el llavero del sistema operativo
#'
#' Si el paquete 'keyring' esta instalado y el sistema tiene un llavero
#' persistente (Administrador de credenciales de Windows, Llavero de macOS
#' o Secret Service en Linux), la clave del asistente IA se guarda alli y no
#' en el archivo de configuracion. Sin llavero, se mantiene el
#' comportamiento anterior (archivo JSON en el directorio de configuracion
#' del usuario).
#'
#' @name secrets
#' @keywords internal
NULL

.rvisual_keyring_service <- "rvisual"

#' Hay un llavero persistente disponible?
#' @return \code{TRUE} si 'keyring' esta instalado y su backend por defecto
#'   guarda las claves de forma persistente.
#' @keywords internal
rvisual_keyring_ok <- function() {
  if (!requireNamespace("keyring", quietly = TRUE)) return(FALSE)
  backend <- tryCatch(keyring::default_backend(), error = function(e) NULL)
  inherits(backend, c("backend_wincred", "backend_macos", "backend_secret_service"))
}

#' Guardar una clave de API en el llavero
#' @param provider Proveedor (se usa como nombre de usuario de la entrada).
#' @param key La clave.
#' @return \code{TRUE} si se guardo en el llavero, \code{FALSE} si no.
#' @keywords internal
rvisual_key_set <- function(provider, key) {
  if (!rvisual_keyring_ok()) return(FALSE)
  tryCatch({
    keyring::key_set_with_value(.rvisual_keyring_service, username = provider,
                                password = key)
    TRUE
  }, error = function(e) FALSE)
}

#' Leer una clave de API del llavero
#' @param provider Proveedor.
#' @return La clave, o \code{NULL} si no esta o no hay llavero.
#' @keywords internal
rvisual_key_get <- function(provider) {
  if (is.null(provider) || !rvisual_keyring_ok()) return(NULL)
  tryCatch(keyring::key_get(.rvisual_keyring_service, username = provider),
           error = function(e) NULL)
}

#' Donde se guarda la clave de API
#' @return Texto para la pantalla de configuracion.
#' @keywords internal
rvisual_key_storage_label <- function() {
  if (rvisual_keyring_ok()) {
    "Tu API key se guarda en el llavero del sistema operativo y nunca se comparte."
  } else {
    paste0("Tu API key se guarda en texto plano en ", config_path(),
           ". Para guardarla en el llavero del sistema, instal\u00e1 el paquete 'keyring'.")
  }
}
