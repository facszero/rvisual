# =============================================================================
# Generador del juego de datos de ejemplo de RVisual: "ech_sintetica"
# =============================================================================
#
# Datos 100% SINTETICOS, inspirados en la estructura de una encuesta de
# hogares (personas dentro de hogares, departamentos, condicion de actividad,
# ingresos). No provienen de microdatos reales ni reproducen estimaciones
# oficiales: las relaciones entre variables son plausibles pero inventadas.
#
# Pensado para probar todos los paneles de RVisual:
#   - Dataset: el mismo contenido en CSV, Excel, RDS, SPSS (.sav) y SQLite.
#   - Explorador: variables numericas, categoricas, fechas y NA.
#   - Constructor: filtrar, ordenar, recodificar, agrupar/resumir y unir
#     (con la tabla 'departamentos').
#   - Base de datos: archivo SQLite con las dos tablas.
#   - Modelado: objetivos para regresion, clasificacion binaria y multiclase,
#     con casos que disparan las reglas del diagnostico.
#
# Uso (desde la raiz del repositorio):
#   Rscript data-raw/generar_ech_sintetica.R
#
# Requiere: writexl, haven, DBI, RSQLite.
# =============================================================================

set.seed(20261123)
n_hogares <- 800
dir_salida <- file.path("inst", "extdata")
dir.create(dir_salida, recursive = TRUE, showWarnings = FALSE)

# -- Tabla de departamentos ---------------------------------------------------
departamentos <- data.frame(
  departamento = 1:19,
  nombre = c("Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia",
             "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
             "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto",
             "San José", "Soriano", "Tacuarembó", "Treinta y Tres"),
  region = c("Montevideo", rep("Interior", 18)),
  stringsAsFactors = FALSE
)
# Peso relativo para sortear hogares (aproximado e ilustrativo)
peso_depto <- c(40, 2, 15, 2.5, 3.5, 1.7, 0.7, 1.9, 1.7, 4.8,
                3.3, 1.6, 3.1, 2, 3.8, 3.3, 2.5, 2.7, 1.4)

# -- Hogares --------------------------------------------------------------------
hogares <- data.frame(
  id_hogar = seq_len(n_hogares),
  departamento = sample(1:19, n_hogares, TRUE, prob = peso_depto),
  estrato = sample(1:5, n_hogares, TRUE, prob = c(0.25, 0.25, 0.2, 0.15, 0.15)),
  personas = sample(1:6, n_hogares, TRUE, prob = c(0.2, 0.3, 0.2, 0.17, 0.08, 0.05))
)
hogares$peso <- round(c(180, 260, 340, 420, 520)[hogares$estrato] *
                        stats::runif(n_hogares, 0.85, 1.15))
hogares$fecha_entrevista <- as.Date("2025-01-01") + sample(0:364, n_hogares, TRUE)
# Localidad: muchos codigos distintos (sirve para la regla de alta cardinalidad)
hogares$localidad <- sprintf("LOC_%03d", sample(1:85, n_hogares, TRUE))

# -- Personas -------------------------------------------------------------------
p <- hogares[rep(seq_len(n_hogares), hogares$personas), ]
n <- nrow(p)
p$id_persona <- seq_len(n)
p$sexo <- sample(c("Hombre", "Mujer"), n, TRUE, prob = c(0.48, 0.52))
p$edad <- pmin(90L, pmax(14L, as.integer(round(
  ifelse(stats::runif(n) < 0.8, stats::rnorm(n, 40, 15), stats::runif(n, 60, 90))))))

# Educacion: anios de estudio segun edad y estrato
anios <- 6 + 0.9 * p$estrato + stats::rnorm(n, 3, 2.5) -
  0.04 * pmax(p$edad - 50, 0) - 3 * (p$edad < 18)
p$anios_educ <- as.integer(pmin(20, pmax(0, round(anios))))
p$nivel_educ <- cut(p$anios_educ, breaks = c(-1, 6, 9, 12, 20),
                    labels = c("Primaria", "Ciclo básico", "Bachillerato", "Terciaria"))
p$nivel_educ <- as.character(p$nivel_educ)

# Condicion de actividad
activo_lp <- -2.3 + 0.25 * pmin(p$edad - 14, 12) - 0.11 * pmax(p$edad - 55, 0) -
  0.6 * (p$sexo == "Mujer") + 0.08 * p$anios_educ
activo <- stats::runif(n) < stats::plogis(activo_lp)
desocup <- activo & stats::runif(n) < stats::plogis(-0.9 - 0.12 * p$anios_educ + 0.5 * (p$edad < 25))
p$condicion_actividad <- ifelse(!activo, "Inactivo", ifelse(desocup, "Desocupado", "Ocupado"))
p$ocupado <- ifelse(p$condicion_actividad == "Ocupado", "Si", "No")
ocup <- p$ocupado == "Si"

# Horas trabajadas (0 si no esta ocupado)
p$horas_trabajadas <- ifelse(ocup, as.integer(pmin(70, pmax(4, round(
  stats::rnorm(n, ifelse(p$sexo == "Hombre", 44, 36), 9))))), 0L)

# Rama de actividad
ramas <- c("Comercio", "Industria", "Servicios", "Construcción", "Agro", "Sector público")
p$rama_actividad <- ifelse(ocup, sample(ramas, n, TRUE,
                                        prob = c(0.2, 0.12, 0.33, 0.08, 0.09, 0.18)),
                           "No aplica")

# Aporta a la seguridad social (relacionado con ocupado, pero no lo determina)
p$aporta_bps <- ifelse(ocup & stats::runif(n) < stats::plogis(-0.8 + 0.2 * p$anios_educ),
                       "Si", "No")

# Ingreso laboral mensual en pesos (0 si no esta ocupado; muy asimetrico)
log_ing <- 9.4 + 0.075 * p$anios_educ + 0.6 * log(pmax(p$horas_trabajadas, 1) / 40) +
  0.18 * (p$sexo == "Hombre") + 0.15 * (p$departamento == 1) +
  0.3 * (p$aporta_bps == "Si") + 0.012 * pmin(p$edad - 18, 30) +
  stats::rnorm(n, 0, 0.5)
p$ingreso_laboral <- ifelse(ocup, round(exp(log_ing), -1), 0)

# -- Faltantes (no respuesta) ---------------------------------------------------
p$ingreso_laboral[ocup & stats::runif(n) < 0.06] <- NA
p$horas_trabajadas[ocup & stats::runif(n) < 0.03] <- NA
p$nivel_educ[stats::runif(n) < 0.01] <- NA

# -- Orden y tipos finales ------------------------------------------------------
ech_sintetica <- p[, c("id_persona", "id_hogar", "fecha_entrevista", "departamento",
                       "localidad", "estrato", "peso", "sexo", "edad", "anios_educ",
                       "nivel_educ", "condicion_actividad", "ocupado",
                       "horas_trabajadas", "rama_actividad", "aporta_bps",
                       "ingreso_laboral")]
rownames(ech_sintetica) <- NULL

# -- Escritura en todos los formatos -------------------------------------------
ruta <- function(x) file.path(dir_salida, x)

write.csv(ech_sintetica, ruta("ech_sintetica.csv"), row.names = FALSE,
          fileEncoding = "UTF-8", na = "")
write.csv(departamentos, ruta("departamentos.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")
saveRDS(ech_sintetica, ruta("ech_sintetica.rds"))
writexl::write_xlsx(list(ech_sintetica = ech_sintetica, departamentos = departamentos),
                    ruta("ech_sintetica.xlsx"))
haven::write_sav(ech_sintetica, ruta("ech_sintetica.sav"))

ruta_db <- ruta("ech_sintetica.sqlite")
if (file.exists(ruta_db)) file.remove(ruta_db)
con <- DBI::dbConnect(RSQLite::SQLite(), ruta_db)
DBI::dbWriteTable(con, "personas", transform(ech_sintetica,
                  fecha_entrevista = as.character(fecha_entrevista)))
DBI::dbWriteTable(con, "departamentos", departamentos)
DBI::dbDisconnect(con)

cat("Filas:", nrow(ech_sintetica), "| Columnas:", ncol(ech_sintetica), "\n")
print(file.info(list.files(dir_salida, full.names = TRUE))[, "size", drop = FALSE])
