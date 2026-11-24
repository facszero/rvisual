# =============================================================================
# Etiquetas de la clasificacion del IPC (division a producto)
# =============================================================================
#
# Extrae codigos y descripciones de un cuadro de indices publicado
# (Cuadro 2.0) y los guarda en data-raw/ipc/clasificacion_ipc.csv.
# Solo se usan codigos y nombres de la clasificacion, que son publicos.
#
# Uso: Rscript data-raw/ipc/00_etiquetas_ipc.R ruta/al/Cuadro2.0.xlsx
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
ruta <- if (length(args) > 0) args[1] else "Cuadro2.0.xlsx"

x <- suppressMessages(readxl::read_excel(ruta, sheet = 1, col_names = FALSE,
                                         col_types = "text"))
x <- as.data.frame(x[, 1:6])
names(x) <- c("DIVISION", "GRUPO", "CLASE", "SUBCLASE", "PRODUCTO", "DESCRIPCION")
x <- x[!is.na(x$DIVISION) & grepl("^[0-9]{2}$", x$DIVISION) & !is.na(x$DESCRIPCION), ]

nivel <- ifelse(!is.na(x$PRODUCTO), "PRODUCTO",
         ifelse(!is.na(x$SUBCLASE), "SUBCLASE",
         ifelse(!is.na(x$CLASE), "CLASE",
         ifelse(!is.na(x$GRUPO), "GRUPO", "DIVISION"))))
codigo <- ifelse(nivel == "PRODUCTO", x$PRODUCTO,
          ifelse(nivel == "SUBCLASE", x$SUBCLASE,
          ifelse(nivel == "CLASE", x$CLASE,
          ifelse(nivel == "GRUPO", x$GRUPO, x$DIVISION))))

etiquetas <- unique(data.frame(NIVEL = nivel, CODIGO = codigo,
                               DESCRIPCION = trimws(x$DESCRIPCION),
                               stringsAsFactors = FALSE))
salida <- file.path("data-raw", "ipc", "clasificacion_ipc.csv")
utils::write.csv(etiquetas, salida, row.names = FALSE, fileEncoding = "UTF-8")
cat("Etiquetas:", nrow(etiquetas), "\n")
print(table(etiquetas$NIVEL))
