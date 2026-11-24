# =============================================================================
# Calibracion del generador sintetico del IPC
# =============================================================================
#
# Lee una salida del calculo (IPC_OUT de un mes) y guarda SOLO PARAMETROS
# AGREGADOS en data-raw/ipc/parametros_ipc.rds. El archivo de entrada NO se
# versiona ni se distribuye; los parametros no contienen NROINE, LOCAL ni
# ninguna fila individual.
#
# Que se calibra y a que nivel:
#   - Variedades: jerarquia, tamano estandar, unidad, IVA, estacionalidad,
#     tamanos relevados, proporcion en dolares, nivel de precio (mediana por
#     region) y dispersion entre establecimientos.
#   - Productos: ponderadores W_I_R, W_I y GAMMA_I_R.
#   - Tipos de empresa: establecimientos por region, tamano de canasta y
#     probabilidad de cada variedad dentro del tipo (asociacion tipo de
#     empresa - producto), semana de relevamiento y parametros de IVA.
#   - Cadenas: distribucion de locales por NROINE.
#   - Dinamica por division: probabilidad de cambio y distribucion de los
#     log-relativos no nulos; componente comun por variedad-region;
#     coordinacion de cadenas.
#   - Faltantes: persistencia (Markov), tasa por grupo de tipo de empresa y
#     razones de falta.
#
# Uso (desde la raiz del repositorio):
#   Rscript data-raw/ipc/01_calibrar_ipc.R ruta/al/IPC_OUT.csv
# =============================================================================

suppressMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
ruta <- if (length(args) > 0) args[1] else "IPC_OUT.csv"
salida <- file.path("data-raw", "ipc", "parametros_ipc.rds")

d <- fread(ruta, sep = ";", dec = ",", showProgress = FALSE)
ne <- d[ESPECIAL == 2]                         # solo productos no especiales
ne[, EST := paste(NROINE, LOCAL)]              # clave interna, no se guarda

# -- Variedades -----------------------------------------------------------------
moda <- function(x) as.numeric(names(which.max(table(x))))
ne[, LP := ifelse(PRECIO_TE_IMP > 0, log(PRECIO_TE_IMP), NA_real_)]
ne[, LP_RES := LP - stats::median(LP, na.rm = TRUE), by = .(VARIEDAD, REGION)]

variedades <- ne[, .(
  DIVISION = moda(DIVISION), GRUPO = moda(GRUPO), CLASE = moda(CLASE),
  SUBCLASE = moda(SUBCLASE), SEISDIG = moda(SEISDIG), PRODUCTO = moda(PRODUCTO),
  TE = moda(TE), UNIMED = moda(UNIMED), IVA_T = moda(IVA_T),
  ESTACIONAL = moda(ESTACIONAL),
  P_DOLAR = mean(MONEDA == 2),
  TR_VALORES = list(as.numeric(names(table(TR)))),
  TR_PROBS = list(as.numeric(prop.table(table(TR)))),
  MU_R1 = stats::median(LP[REGION == 1], na.rm = TRUE),
  MU_R2 = stats::median(LP[REGION == 2], na.rm = TRUE),
  SD_LP = stats::sd(LP_RES, na.rm = TRUE)
), by = VARIEDAD]
variedades[is.na(MU_R1), MU_R1 := MU_R2]
variedades[is.na(MU_R2), MU_R2 := MU_R1]
sd_div <- ne[, .(SD_DIV = stats::sd(LP_RES, na.rm = TRUE)), by = DIVISION]
variedades <- sd_div[variedades, on = "DIVISION"]
variedades[is.na(SD_LP) | SD_LP < 0.02, SD_LP := SD_DIV]
variedades[, SD_DIV := NULL]
variedades <- variedades[is.finite(MU_R1)]

# -- Productos: ponderadores ----------------------------------------------------
productos <- unique(ne[, .(PRODUCTO, REGION, W_I_R, W_I, GAMMA_I_R)])
productos <- productos[, .SD[1], by = .(PRODUCTO, REGION)]

# -- Establecimientos y tipos de empresa ----------------------------------------
est <- ne[, .(TIPOEMP = moda(TIPOEMP), REGION = REGION[1], SEMREP = moda(SEMREP),
              NROINE = NROINE[1], N_VAR = uniqueN(VARIEDAD),
              IVA_D = IVA_D[1], IVA_C = IVA_C[1],
              REG_IVA = as.integer(IVA_DC[1] > 0)), by = EST]

tipos <- est[, .(
  N_R1 = sum(REGION == 1), N_R2 = sum(REGION == 2),
  TAMANOS = list(N_VAR),
  SEMREP_PROBS = list(as.numeric(prop.table(table(factor(SEMREP, levels = 1:4))))),
  IVA_DC_PAR = list(utils::head(unique(round(cbind(IVA_D, IVA_C), 4)), 25)),
  P_REG_IVA = mean(REG_IVA)
), by = TIPOEMP]

# Probabilidad de cada variedad dentro del tipo de empresa
canasta <- unique(ne[, .(EST, VARIEDAD)])[est[, .(EST, TIPOEMP)], on = "EST"]
canasta <- canasta[, .(N = .N), by = .(TIPOEMP, VARIEDAD)]
canasta <- canasta[est[, .(N_EST = .N), by = TIPOEMP], on = "TIPOEMP"]
canasta[, P := N / N_EST]
canasta <- canasta[VARIEDAD %in% variedades$VARIEDAD, .(TIPOEMP, VARIEDAD, P)]

# -- Cadenas ----------------------------------------------------------------------
locales <- est[, .(N_LOC = .N, GRUPO_TE = TIPOEMP[1] %/% 100), by = NROINE]
cadenas <- locales[, .(LOCALES = list(pmin(N_LOC, 30L))), by = GRUPO_TE]

# -- Dinamica ---------------------------------------------------------------------
o <- ne[A_IMPUTAR == 0 & A_IMPUTAR_ANT == 0 & PRECIO_TE_IMP_ANT > 0 & PRECIO_TE_IMP > 0]
o[, Y := log(PRECIO_TE_IMP / PRECIO_TE_IMP_ANT)]
o[, CAMBIA := abs(Y) > 1e-9]
# Precios en dolares: la rigidez se mide en su moneda (el tipo de cambio se
# simula aparte); en pesos, sobre el precio en tamano estandar.
o[, CAMBIA_REL := PRECIO_REL > 0 & PRECIO_REL_ANT > 0 & abs(log(PRECIO_REL / PRECIO_REL_ANT)) > 1e-9]
p_usd_global <- o[MONEDA == 2, mean(CAMBIA_REL)]
dinamica <- o[, .(
  P_CAMBIO = mean(CAMBIA[MONEDA == 1]),
  P_CAMBIO_USD = if (sum(MONEDA == 2) >= 20) mean(CAMBIA_REL[MONEDA == 2]) else p_usd_global,
  Q_CAMBIO = list(as.numeric(stats::quantile(Y[CAMBIA & MONEDA == 1], probs = seq(0, 1, 0.01))))
), by = DIVISION]

ch <- o[CAMBIA == TRUE]
ch[, M_VR := mean(Y), by = .(VARIEDAD, REGION)]
share_comun <- stats::var(ch$M_VR) / stats::var(ch$Y)

o[, N_LOC := .N, by = .(NROINE, VARIEDAD, REGION)]
cc <- o[N_LOC >= 3]
cc[, MODA_Y := as.numeric(names(which.max(table(round(Y, 6))))), by = .(NROINE, VARIEDAD, REGION)]
p_sigue_cadena <- mean(abs(cc$Y - cc$MODA_Y) < 1e-6)

p_efecto_est <- {
  p <- ne[is.finite(LP_RES)]
  p[, EFE := mean(LP_RES), by = EST]
  stats::var(p$EFE) / stats::var(p$LP_RES)
}

redondeo <- list(
  P_ENTERO_MN  = mean(o[MONEDA == 1, PRECIO_REL == round(PRECIO_REL)]),
  P_ENTERO_USD = mean(o[MONEDA == 2, PRECIO_REL == round(PRECIO_REL)])
)

# -- Faltantes ----------------------------------------------------------------------
# Dos capas: el establecimiento entero sin respuesta (Markov propio) y, en
# los establecimientos que responden, cada fila.
ne[, `:=`(TODO_T = all(A_IMPUTAR == 1), TODO_ANT = all(A_IMPUTAR_ANT == 1)), by = EST]
e_t <- ne[, .(TODO_T = TODO_T[1], TODO_ANT = TODO_ANT[1], N = .N), by = EST]
cortes_tamano <- c(0, 1, 4, 13, Inf)            # 1, 2-4, 5-13, 14+ variedades
e_t[, TAM := cut(N, cortes_tamano, labels = FALSE)]
parcial <- ne[TODO_T == FALSE & TODO_ANT == FALSE]
tr <- prop.table(table(parcial$A_IMPUTAR_ANT, parcial$A_IMPUTAR), 1)
faltantes <- list(
  CORTES_TAMANO = cortes_tamano,
  # P11: persistencia medida. P01: se fija para que la proporcion de largo
  # plazo coincida con la observada en el corte (P_T), porque un solo mes no
  # alcanza para estimar bien la tasa de entrada.
  P_EST = e_t[, .(P_T = mean(TODO_T),
                  P11 = if (any(TODO_ANT)) mean(TODO_T[TODO_ANT]) else NA_real_),
              by = TAM][order(TAM)][, P01 := P_T * (1 - P11) / (1 - P_T)][],
  P01 = tr["0", "1"], P11 = tr["1", "1"],
  TASA_GRUPO = parcial[, .(TASA = mean(A_IMPUTAR)), by = .(GRUPO_TE = TIPOEMP %/% 100)],
  RAZONES = ne[A_IMPUTAR == 1 & RAZONFALTA > 0 & ESTACIONAL == 0, .(P = .N), by = RAZONFALTA][, P := P / sum(P)][],
  # Variedades estacionales: fuera de temporada faltan en todos los comercios
  RAZONES_ESTACIONAL = ne[A_IMPUTAR == 1 & RAZONFALTA > 0 & ESTACIONAL == 1, .(P = .N), by = RAZONFALTA][, P := P / sum(P)][]
)

# -- Guardar ------------------------------------------------------------------------
param <- list(
  meta = list(
    fuente = "IPC_OUT de un mes (datos alterados respecto de produccion)",
    aniomes = unique(d$ANIOMES), qmin = unique(d$QMIN)[1],
    cotizacion = stats::median(d$COTIZACION), creado = Sys.Date()
  ),
  variedades = variedades, productos = productos, tipos = tipos,
  canasta = canasta, cadenas = cadenas, dinamica = dinamica,
  share_comun = share_comun, p_sigue_cadena = p_sigue_cadena,
  p_efecto_est = p_efecto_est, redondeo = redondeo, faltantes = faltantes,
  # Supuestos que un solo mes no permite medir (ajustables)
  supuestos = list(
    inflacion_anual = 0.0455,      # variacion interanual publicada (Cuadro 2.0, agosto 2026)
    amplitud_estacional = 0.10,    # +-10% en log-precio para variedades estacionales
    sd_cotizacion_mensual = 0.015, # paseo aleatorio del tipo de cambio
    factor_mnar = 1.3,             # faltan 30% mas cuando el precio cambio
    reversion = 0.3                # en cada cambio, se corrige 30% del desvio respecto del nivel propio
  )
)
dir.create(dirname(salida), recursive = TRUE, showWarnings = FALSE)
saveRDS(param, salida)
cat("Parametros guardados en", salida, "\n")
cat("Variedades:", nrow(variedades), "| Tipos de empresa:", nrow(tipos),
    "| Establecimientos de referencia:", nrow(est), "\n")
