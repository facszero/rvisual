# =============================================================================
# Generador del juego de datos SINTETICO del IPC (productos no especiales)
# =============================================================================
#
# Genera un panel mensual informante-variedad con las mismas columnas que la
# salida del calculo (IPC_OUT), calibrado con parametros AGREGADOS
# (data-raw/ipc/parametros_ipc.rds, ver 01_calibrar_ipc.R). Ningun
# establecimiento, canasta o precio corresponde a uno real:
#   - NROINE son numeros de 9 digitos (desde 900.000.001), fuera del rango
#     real; LOCAL se numera 1, 2, ... dentro de cada NROINE.
#   - TIPOEMP son los codigos reales (estan asociados a los productos).
#   - Las canastas se sortean con la probabilidad de cada variedad dentro de
#     su tipo de empresa; los precios se simulan desde cero.
#
# El calculo se emula como en produccion: los precios faltantes se imputan con
# la cascada F1-F5 (QMIN = 1/3) y el precio imputado pasa a t-1 en el mes
# siguiente. Ademas se guarda el PRECIO VERDADERO de cada faltante
# (PRECIO_TE_VERDADERO), que con datos reales nunca se conoce.
#
# Salidas (carpeta datos-ipc/):
#   ipc_sintetico_panel.rds             panel mensual, formato IPC_OUT
#   IPC_OUT_SINTETICO_<aniomes>.csv     ultimo mes, mismo formato que el export
#   ipc_sintetico_pares.rds             tabla lista para Machine Learning
#   (con --csv, ademas ipc_sintetico_panel.csv.gz e ipc_sintetico_pares.csv.gz)
#   clasificacion_ipc.csv               etiquetas division ... producto
#
# Uso (desde la raiz del repositorio):
#   Rscript data-raw/ipc/02_generar_ipc.R            # 700 establecimientos
#   Rscript data-raw/ipc/02_generar_ipc.R 3400       # tamano real aproximado
#   Rscript data-raw/ipc/02_generar_ipc.R 700 --csv  # tambien CSV completos
# =============================================================================

suppressMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
cfg <- list(
  n_establecimientos = if (length(grep("^[0-9]+$", args)) > 0)
    as.integer(grep("^[0-9]+$", args, value = TRUE)[1]) else 700L,
  primer_mes = as.Date("2023-10-01"),  # mes base (todo observado); no se exporta
  n_meses    = 37L,                    # base + 36 meses exportados
  semilla    = 20261124L,
  salida     = "datos-ipc",
  csv_completo = "--csv" %in% args    # CSV del panel y de los pares completos (pesados)
)
set.seed(cfg$semilla)

p   <- readRDS(file.path("data-raw", "ipc", "parametros_ipc.rds"))
etq <- fread(file.path("data-raw", "ipc", "clasificacion_ipc.csv"),
             colClasses = "character", encoding = "UTF-8")
qmin <- p$meta$qmin
sup  <- p$supuestos

pad  <- function(x, n) formatC(as.numeric(x), width = n, flag = "0", format = "f", digits = 0)
pick <- function(x) x[sample.int(length(x), 1L)]
`%||%` <- function(a, b) if (is.null(a)) b else a
sround <- function(x) as.integer(floor(x) + (stats::runif(length(x)) < x - floor(x)))

# -- 1. Establecimientos y cadenas ---------------------------------------------
tipos <- copy(p$tipos)
frac  <- cfg$n_establecimientos / sum(tipos$N_R1 + tipos$N_R2)
tipos[, `:=`(n1 = sround(N_R1 * frac), n2 = sround(N_R2 * frac))]
# Todo tipo de empresa real aparece al menos una vez
tipos[n1 + n2 == 0, `:=`(n1 = as.integer(N_R1 >= N_R2), n2 = as.integer(N_R2 > N_R1))]

est <- tipos[, .(REGION = sample(c(rep(1L, n1), rep(2L, n2)))), by = TIPOEMP]
tam_cadena <- setNames(p$cadenas$LOCALES, p$cadenas$GRUPO_TE)

asignar_cadenas <- function(n, tamanos) {
  ids <- integer(n); i <- 1L; k <- 0L
  while (i <= n) {
    s <- min(pick(tamanos), n - i + 1L)
    k <- k + 1L; ids[i:(i + s - 1L)] <- k; i <- i + s
  }
  ids
}
est[, CADENA_LOCAL := asignar_cadenas(.N, tam_cadena[[as.character(TIPOEMP[1] %/% 100)]] %||% 1L),
    by = TIPOEMP]
est[, CADENA := .GRP, by = .(TIPOEMP, CADENA_LOCAL)]
n_cadenas <- max(est$CADENA)
nroine <- 900000000 + sample.int(99999999L, n_cadenas)
est[, NROINE := nroine[CADENA]]
est[, LOCAL := seq_len(.N), by = CADENA]
est[, EST_ID := .I]

# Atributos por establecimiento
tipos_lista <- split(tipos, by = "TIPOEMP")
est[, SEMREP := {
  pr <- tipos_lista[[as.character(TIPOEMP[1])]]$SEMREP_PROBS[[1]]
  sample(1:4, .N, replace = TRUE, prob = pr + 1e-9)
}, by = TIPOEMP]
est[, c("IVA_D", "IVA_C", "REG_IVA") := {
  t <- tipos_lista[[as.character(TIPOEMP[1])]]
  m <- t$IVA_DC_PAR[[1]]
  f <- sample.int(nrow(m), .N, replace = TRUE)
  list(m[f, 1], m[f, 2], as.integer(stats::runif(.N) < t$P_REG_IVA))
}, by = TIPOEMP]
z_cadena <- stats::rnorm(n_cadenas)
est[, Z_CADENA := z_cadena[CADENA]]

# -- 2. Canastas ---------------------------------------------------------------
V <- copy(p$variedades)
canasta <- p$canasta[VARIEDAD %in% V$VARIEDAD]
cand <- split(canasta, by = "TIPOEMP")

cad <- unique(est[, .(CADENA, TIPOEMP)])
cesta_cadena <- cad[, {
  cd <- cand[[as.character(TIPOEMP)]]
  n  <- pick(tipos_lista[[as.character(TIPOEMP)]]$TAMANOS[[1]])
  k  <- min(n, nrow(cd))
  list(VARIEDAD = if (k > 0) cd$VARIEDAD[sample.int(nrow(cd), k, prob = cd$P)] else numeric(0))
}, by = .(CADENA, TIPOEMP)]

# Perturbacion de canastas: evita reproducir la canasta exacta de un
# establecimiento real en tipos de empresa con pocos establecimientos.
pool_grupo <- canasta[, .(VARIEDAD = unique(VARIEDAD)), by = .(GRUPO_TE = TIPOEMP %/% 100)]
cesta_cadena <- cesta_cadena[stats::runif(.N) >= 0.10 | .N == 1, .SD, by = CADENA]
extra <- cad[, {
  pool <- pool_grupo[GRUPO_TE == TIPOEMP %/% 100, VARIEDAD]
  k <- min(stats::rpois(1, 1) + 1L, length(pool))
  list(VARIEDAD = if (k > 0) pool[sample.int(length(pool), k)] else numeric(0))
}, by = .(CADENA, TIPOEMP)]
cesta_cadena <- unique(rbind(cesta_cadena[, .(CADENA, TIPOEMP, VARIEDAD)],
                             extra[, .(CADENA, TIPOEMP, VARIEDAD)]))

filas <- est[, .(EST_ID, CADENA, REGION)][cesta_cadena[, .(CADENA, VARIEDAD)],
                                          on = "CADENA", allow.cartesian = TRUE]
filas[, N_LOC := uniqueN(EST_ID), by = CADENA]
filas <- filas[N_LOC == 1 | stats::runif(.N) < 0.9]       # locales de una cadena: 90% de su canasta
filas <- filas[est[, .(EST_ID, TIPOEMP, NROINE, LOCAL, SEMREP, IVA_D, IVA_C, REG_IVA, Z_CADENA)],
               on = "EST_ID", nomatch = 0]
filas <- V[filas, on = "VARIEDAD"]

# Atributos por cadena-variedad (coherentes entre locales)
cv <- unique(filas[, .(CADENA, VARIEDAD)])
cv <- V[, .(VARIEDAD, TR_VALORES, TR_PROBS, P_DOLAR)][cv, on = "VARIEDAD"]
cv[, TR := mapply(function(v, pr) v[sample.int(length(v), 1L, prob = pr)], TR_VALORES, TR_PROBS)]
cv[, MONEDA := 1L + as.integer(stats::runif(.N) < P_DOLAR)]
cv[, EPS_CV := stats::rnorm(.N)]
filas <- cv[, .(CADENA, VARIEDAD, TR, MONEDA, EPS_CV)][filas, on = .(CADENA, VARIEDAD)]

# Ponderadores del producto
filas <- p$productos[filas, on = .(PRODUCTO, REGION)]
filas[is.na(W_I_R), `:=`(W_I_R = 0, W_I = 0, GAMMA_I_R = 0)]

# IVA por debito / credito
filas[, `:=`(IVA_DC = ifelse(REG_IVA == 1, 0.09, 0),
             IVA_DD = ifelse(REG_IVA == 1, 0.09, ifelse(IVA_T > 0, 0.02, 0)))]

# Unidad de decision de precios: cadena x variedad x region
filas[, UNIDAD := .GRP, by = .(CADENA, VARIEDAD, REGION)]
filas[, EN_CADENA := uniqueN(EST_ID) > 1, by = UNIDAD]
setorder(filas, EST_ID, VARIEDAD)
filas[, FILA := .I]

# -- 3. Tipo de cambio ---------------------------------------------------------
meses <- seq(cfg$primer_mes, by = "month", length.out = cfg$n_meses)
cot <- numeric(cfg$n_meses)
cot[cfg$n_meses] <- p$meta$cotizacion
for (t in (cfg$n_meses - 1):1) cot[t] <- cot[t + 1] * exp(-stats::rnorm(1, 0, sup$sd_cotizacion_mensual))
cot <- round(cot, 2)

# -- 4. Precio inicial ---------------------------------------------------------
redondear <- function(x, usd) {
  ifelse(usd, ifelse(x >= 50, round(x), round(x, 2)), ifelse(x >= 10, round(x), round(x, 2)))
}
pe <- p$p_efecto_est
filas[, MU := ifelse(REGION == 1, MU_R1, MU_R2)]
filas[, LP0 := MU + SD_LP * (sqrt(pe) * Z_CADENA +
                             sqrt(1 - pe) * (0.9 * EPS_CV + sqrt(0.19) * stats::rnorm(.N)))]
filas[, USD := MONEDA == 2]
filas[, P_VERD := redondear(exp(LP0) * TR / TE / ifelse(USD, cot[1], 1), USD)]
filas[P_VERD <= 0, P_VERD := ifelse(USD, 0.01, 0.01)]

# -- 5. Parametros de dinamica y faltantes ------------------------------------
din <- copy(p$dinamica)
din[, MEDIA := vapply(Q_CAMBIO, mean, numeric(1))]
din[, VAR := vapply(Q_CAMBIO, stats::var, numeric(1))]
qtab <- setNames(din$Q_CAMBIO, din$DIVISION)
filas <- din[, .(DIVISION, P_CAMBIO_MN = P_CAMBIO, P_CAMBIO_USD, MEDIA, VAR)][filas, on = "DIVISION"]
filas[, P_CAMBIO := ifelse(MONEDA == 2, P_CAMBIO_USD, P_CAMBIO_MN)]

g <- log(1 + sup$inflacion_anual) / 12
sh <- p$share_comun
sorteo_cambio <- function(div) {
  out <- numeric(length(div))
  for (dv in unique(div)) {
    i <- which(div == dv)
    out[i] <- stats::approx(seq(0, 1, 0.01), qtab[[as.character(dv)]], stats::runif(length(i)))$y
  }
  out
}

tasa <- setNames(p$faltantes$TASA_GRUPO$TASA, p$faltantes$TASA_GRUPO$GRUPO_TE)
P11 <- p$faltantes$P11
p_est <- p$faltantes$P_EST
p_est[is.na(P11), P11 := stats::weighted.mean(p_est$P11, !is.na(p_est$P11), na.rm = TRUE)]
tam_est <- filas[, .N, by = EST_ID]
est[tam_est, on = "EST_ID", N_FILAS := i.N]
est[is.na(N_FILAS), N_FILAS := 0L]
est[, TAM := pmax(1L, cut(pmax(N_FILAS, 1L), p$faltantes$CORTES_TAMANO, labels = FALSE))]
est[p_est, on = "TAM", `:=`(P_EST_01 = i.P01, P_EST_11 = i.P11)]
kappa <- sup$reversion
filas[, PI_G := tasa[as.character(TIPOEMP %/% 100)]]
filas[is.na(PI_G), PI_G := mean(tasa)]
filas[, P01 := PI_G * (1 - P11) / (1 - PI_G)]
fm <- sup$factor_mnar
filas[, P01 := P01 / (1 + (fm - 1) * P_CAMBIO)]
razones <- p$faltantes$RAZONES
razones_est <- p$faltantes$RAZONES_ESTACIONAL
# Temporada de cada variedad estacional: invierno (en venta abril-setiembre)
# o verano (octubre-marzo). Fuera de temporada falta en todos los comercios.
temporada <- V[ESTACIONAL == 1, .(VARIEDAD, INVIERNO = stats::runif(.N) < 0.5)]
filas <- temporada[filas, on = "VARIEDAD"]
filas[is.na(INVIERNO), INVIERNO := NA]
fuera_temporada <- function(mes) {
  invierno_en_venta <- mes %in% 4:9
  ifelse(is.na(filas$INVIERNO), FALSE, ifelse(filas$INVIERNO, !invierno_en_venta, invierno_en_venta))
}

# -- 6. Emulacion del calculo: cascada de imputacion ---------------------------
gm <- function(x) exp(mean(log(x)))
cascada <- function(m) {
  # Reproduce el calculo de produccion (verificado contra un IPC_OUT real):
  #   F1 variedad x tipo de empresa x region  (solo divisiones 01 y 02)
  #   F2 variedad x region
  #   F3 producto x region
  #   F4 subclase x region: media geometrica de F3 ponderada por W_I_R
  #   F5 clase x region:    media geometrica de F4 ponderada
  #   F6 sin datos: 1
  # Fk = media geometrica de los relativos de los pares observados en t y t-1.
  # Q1-Q3 = pares observados / filas de la celda. Q4 = peso (W_I_R) de los
  # productos con Q3 >= QMIN sobre el peso de la subclase; Q5 analogo con
  # las subclases. Se usa el primer nivel con Q >= QMIN.
  # Verificacion contra produccion: F1, F2, F3, Q1-Q4 identicos; F4 es una
  # aproximacion (correlacion 0,99): la ponderacion exacta usa informacion que
  # no esta en la salida del calculo.
  m[, PAR := A == 0L & A_ANT == 0L]
  m[, LR := ifelse(PAR, log(PTE_IMP / PTE_IMP_ANT), NA_real_)]
  niv <- function(by) m[, .(N_OBS = sum(PAR), N_TOT = .N,
                            F = if (any(PAR)) exp(mean(LR[PAR])) else NA_real_), by = by]
  wgeo <- function(w, f, k) if (any(k & w > 0)) exp(sum((w * log(f))[k & w > 0]) / sum(w[k & w > 0])) else NA_real_
  c1 <- niv(c("VARIEDAD", "TIPOEMP", "REGION"))[, Q := N_OBS / N_TOT]
  c2 <- niv(c("VARIEDAD", "REGION"))[, Q := N_OBS / N_TOT]
  c3 <- niv(c("PRODUCTO", "REGION"))[, Q := N_OBS / N_TOT]
  w3 <- unique(m[, .(PRODUCTO, REGION, SUBCLASE, CLASE, W = W_I_R)])[, .SD[1], by = .(PRODUCTO, REGION)]
  c3 <- w3[c3, on = .(PRODUCTO, REGION)]
  c4 <- c3[, .(Q = if (sum(W) > 0) sum(W[Q >= qmin - 1e-12]) / sum(W) else NA_real_,
               F = wgeo(W, F, Q >= qmin - 1e-12 & N_OBS > 0),
               W = sum(W), CLASE = CLASE[1]), by = .(SUBCLASE, REGION)]
  c5 <- c4[, .(Q = if (sum(W) > 0) sum(W[!is.na(Q) & Q >= qmin - 1e-12]) / sum(W) else NA_real_,
               F = wgeo(W, F, !is.na(Q) & Q >= qmin - 1e-12 & !is.na(F))), by = .(CLASE, REGION)]
  pega <- function(cel, by, k) {
    x <- cel[, c(by, "F", "Q"), with = FALSE]
    setnames(x, c("F", "Q"), paste0(c("F", "Q"), k))
    m[x, on = by, (paste0(c("F", "Q"), k)) := mget(paste0("i.", c("F", "Q"), k))]
  }
  pega(c1, c("VARIEDAD", "TIPOEMP", "REGION"), 1)
  pega(c2, c("VARIEDAD", "REGION"), 2)
  pega(c3, c("PRODUCTO", "REGION"), 3)
  pega(c4, c("SUBCLASE", "REGION"), 4)
  pega(c5, c("CLASE", "REGION"), 5)
  m[!(DIVISION %in% c(1, 2)), `:=`(F1 = NA_real_, Q1 = NA_real_)]
  ok <- function(k) !is.na(m[[paste0("F", k)]]) & !is.na(m[[paste0("Q", k)]]) &
                    m[[paste0("Q", k)]] >= qmin - 1e-12
  nivel <- ifelse(ok(1), 1L, ifelse(ok(2), 2L, ifelse(ok(3), 3L, ifelse(ok(4), 4L,
           ifelse(ok(5), 5L, 6L)))))
  fimp <- ifelse(nivel == 1L, m$F1, ifelse(nivel == 2L, m$F2, ifelse(nivel == 3L, m$F3,
          ifelse(nivel == 4L, m$F4, ifelse(nivel == 5L, m$F5, 1)))))
  m[, NIVELIMP := ifelse(A == 1L, nivel, 0L)]
  m[, F_IMP := ifelse(A == 1L, fimp, NA_real_)]
  m[]
}

# -- 7. Simulacion mes a mes ----------------------------------------------------
estado <- filas[, .(FILA, P_VERD, USD, TE, TR)]
estado[, `:=`(MN_VERD = P_VERD * ifelse(USD, cot[1], 1))]
estado[, `:=`(PTE_VERD = MN_VERD * TE / TR, PTE_IMP = MN_VERD * TE / TR, MN_IMP = MN_VERD,
              A = 0L, N_IMPUT = 0L, P_REL = P_VERD, RAZON = 0L)]
# Posicion propia de cada fila respecto de su variedad-region (para la reversion)
desvio <- function(lp) lp - stats::ave(lp, filas$VARIEDAD, filas$REGION, FUN = stats::median)
estado[, OFFSET := desvio(log(PTE_VERD))]
est_falta <- rep(FALSE, nrow(est))

salidas <- vector("list", cfg$n_meses - 1L)
for (t in 2:cfg$n_meses) {
  mes <- as.integer(format(meses[t], "%m"))
  ant <- copy(estado)

  # Decision por unidad (cadena x variedad x region)
  u <- filas[, .(DIVISION = DIVISION[1], P_CAMBIO = P_CAMBIO[1]), by = UNIDAD]
  u[, `:=`(CH_U = stats::runif(.N) < P_CAMBIO, E_U = sorteo_cambio(DIVISION))]
  vr <- unique(filas[, .(VARIEDAD, REGION, VAR)])
  vr[, C_VR := stats::rnorm(.N, 0, sqrt(sh * VAR))]

  x <- filas[, .(FILA, UNIDAD, EN_CADENA, VARIEDAD, REGION, DIVISION, P_CAMBIO, MEDIA, VAR,
                 ESTACIONAL, P01, USD, TE, TR)]
  x <- u[, .(UNIDAD, CH_U, E_U)][x, on = "UNIDAD"]
  x <- vr[, .(VARIEDAD, REGION, C_VR)][x, on = .(VARIEDAD, REGION)]
  setorder(x, FILA)
  sigue <- x$EN_CADENA & stats::runif(nrow(x)) < p$p_sigue_cadena
  ch <- ifelse(sigue, x$CH_U, stats::runif(nrow(x)) < x$P_CAMBIO)
  e  <- ifelse(sigue, x$E_U, sorteo_cambio(x$DIVISION))
  seas <- ifelse(x$ESTACIONAL == 1,
                 sup$amplitud_estacional * (sin(2 * pi * mes / 12) - sin(2 * pi * (mes - 1) / 12)), 0)
  dev <- desvio(log(ant$PTE_VERD)) - ant$OFFSET
  y <- x$C_VR + (e - x$MEDIA) * sqrt(1 - sh) + g / x$P_CAMBIO + seas - kappa * dev

  p_nuevo <- ifelse(ch, redondear(ant$P_VERD * exp(y), x$USD), ant$P_VERD)
  p_nuevo <- pmax(p_nuevo, 0.01)
  cambio_real <- p_nuevo != ant$P_VERD
  mn_verd <- p_nuevo * ifelse(x$USD, cot[t], 1)
  pte_verd <- mn_verd * x$TE / x$TR

  # Faltantes: establecimiento entero (Markov) y filas (Markov + MNAR)
  est_falta <- ifelse(est_falta, stats::runif(nrow(est)) < est$P_EST_11,
                                 stats::runif(nrow(est)) < est$P_EST_01)
  todo <- est_falta[filas$EST_ID]
  p_miss <- ifelse(ant$A == 1L, P11, x$P01 * ifelse(cambio_real, fm, 1))
  fuera <- fuera_temporada(mes)
  a <- as.integer(todo | fuera | stats::runif(nrow(x)) < p_miss)
  razon <- ifelse(a == 1L, razones$RAZONFALTA[sample.int(nrow(razones), nrow(x), TRUE, razones$P)], 0L)
  razon <- ifelse(fuera, razones_est$RAZONFALTA[sample.int(nrow(razones_est), nrow(x), TRUE, razones_est$P)], razon)

  # Calculo
  m <- filas[, .(FILA, VARIEDAD, PRODUCTO, SUBCLASE, CLASE, DIVISION, TIPOEMP, REGION, W_I_R)]
  m[, `:=`(A = a, A_ANT = ant$A, PTE_IMP_ANT = ant$PTE_IMP,
           PTE_IMP = ifelse(a == 0L, pte_verd, NA_real_))]
  m <- cascada(m)
  setorder(m, FILA)
  pte_imp <- ifelse(a == 0L, pte_verd, ant$PTE_IMP * m$F_IMP)
  mn_imp  <- ifelse(a == 0L, mn_verd, ant$MN_IMP * m$F_IMP)

  salidas[[t - 1L]] <- data.table(
    FILA = filas$FILA, ANIOMES = as.integer(format(meses[t], "%Y%m")),
    COTIZACION = cot[t], COTIZACION_ANT = cot[t - 1L],
    RAZONFALTA = razon, RAZONFALTA_ANT = ant$RAZON,
    PRECIO_REL = ifelse(a == 0L, p_nuevo, 0), PRECIO_REL_ANT = ant$P_REL,
    PRECIO_MN = ifelse(a == 0L, mn_verd, 0), PRECIO_MN_IMP = mn_imp, PRECIO_MN_IMP_ANT = ant$MN_IMP,
    PRECIO_TE = ifelse(a == 0L, pte_verd, 0),
    PRECIO_TE_IMP = pte_imp, PRECIO_TE_IMP_ANT = ant$PTE_IMP,
    A_IMPUTAR = a, A_IMPUTAR_ANT = ant$A,
    F_IMP = m$F_IMP, QMIN = qmin,
    Q1 = m$Q1, Q2 = m$Q2, Q3 = m$Q3, Q4 = m$Q4, Q5 = m$Q5,
    F1 = m$F1, F2 = m$F2, F3 = m$F3, F4 = m$F4, F5 = m$F5,
    N_IMPUT = ifelse(a == 1L, ant$N_IMPUT + 1L, 0L), N_IMPUT_ANT = ant$N_IMPUT,
    NIVELIMP = m$NIVELIMP,
    PRECIO_TE_VERDADERO = pte_verd
  )

  estado[, `:=`(P_VERD = p_nuevo, MN_VERD = mn_verd, PTE_VERD = pte_verd,
                PTE_IMP = pte_imp, MN_IMP = mn_imp, A = a,
                N_IMPUT = ifelse(a == 1L, ant$N_IMPUT + 1L, 0L),
                P_REL = ifelse(a == 0L, p_nuevo, 0), RAZON = razon)]
  if (t %% 6 == 0) cat("Mes", format(meses[t], "%Y-%m"), "\n")
}

# -- 8. Panel en formato IPC_OUT ------------------------------------------------
panel <- rbindlist(salidas)
fijas <- filas[, .(FILA, SEMREP, REGION, DIVISION, GRUPO, CLASE, SUBCLASE, SEISDIG, PRODUCTO,
                   VARIEDAD, NROINE, LOCAL, TIPOEMP, W_I_R, W_I, GAMMA_I_R, IVA_D, IVA_C,
                   IVA_DC, IVA_DD, IVA_T, TR, TE, UNIMED, ESTACIONAL, MONEDA)]
panel <- fijas[panel, on = "FILA"]
panel[, `:=`(ANIO = ANIOMES %/% 100L, MES = ANIOMES %% 100L, SEMCALC = 4L, ESTADO = 1L,
             COMPONENTE = 0L, ESPECIAL = 2L, FH = 1)]
panel[, DESC := (IVA_D * IVA_DD + IVA_C * IVA_DC) / (1 + IVA_T)]
panel[, `:=`(PRECIO_FIN = PRECIO_TE_IMP * (1 - DESC), PRECIO_FIN_ANT = PRECIO_TE_IMP_ANT * (1 - DESC))]
panel[, RELATIVO_VJ := PRECIO_TE_IMP / PRECIO_TE_IMP_ANT]
panel[, DESC := NULL]

codigos <- c(DIVISION = 2, GRUPO = 3, CLASE = 4, SUBCLASE = 5, SEISDIG = 6, PRODUCTO = 8, VARIEDAD = 10)
for (cc in names(codigos)) set(panel, j = cc, value = pad(panel[[cc]], codigos[[cc]]))

orden <- c("ANIOMES", "ANIO", "MES", "SEMREP", "SEMCALC", "ESTADO", "REGION", "DIVISION", "GRUPO",
           "CLASE", "SUBCLASE", "SEISDIG", "PRODUCTO", "VARIEDAD", "COMPONENTE", "NROINE", "LOCAL",
           "TIPOEMP", "W_I_R", "W_I", "GAMMA_I_R", "IVA_D", "IVA_C", "IVA_DC", "IVA_DD", "IVA_T", "FH",
           "COTIZACION", "COTIZACION_ANT", "TR", "TE", "UNIMED", "RAZONFALTA", "RAZONFALTA_ANT",
           "ESTACIONAL", "ESPECIAL", "MONEDA", "PRECIO_REL", "PRECIO_REL_ANT", "PRECIO_MN",
           "PRECIO_MN_IMP", "PRECIO_MN_IMP_ANT", "PRECIO_TE_IMP", "PRECIO_TE_IMP_ANT", "PRECIO_FIN",
           "PRECIO_FIN_ANT", "A_IMPUTAR", "A_IMPUTAR_ANT", "F_IMP", "QMIN", "Q1", "Q2", "Q3", "Q4",
           "Q5", "N_IMPUT", "N_IMPUT_ANT", "NIVELIMP", "PRECIO_TE", "RELATIVO_VJ",
           "F1", "F2", "F3", "F4", "F5", "PRECIO_TE_VERDADERO")
setkey(panel, ANIOMES, NROINE, LOCAL, VARIEDAD)
fila_id <- panel$FILA
panel <- panel[, ..orden]

# -- 9. Tabla de pares para Machine Learning ----------------------------------
desc <- function(nivel, cod) etq[NIVEL == nivel][match(cod, CODIGO), DESCRIPCION]
pares <- panel[, .(
  ANIOMES, MES, REGION, DIVISION, DESC_DIVISION = desc("DIVISION", DIVISION),
  GRUPO, CLASE, SUBCLASE, PRODUCTO, DESC_PRODUCTO = desc("PRODUCTO", PRODUCTO), VARIEDAD,
  NROINE, LOCAL, TIPOEMP, GRUPO_TIPOEMP = TIPOEMP %/% 100L, MONEDA, ESTACIONAL, IVA_T,
  A_IMPUTAR, A_IMPUTAR_ANT, NIVELIMP,
  LOG_REL = ifelse(A_IMPUTAR == 0 & A_IMPUTAR_ANT == 0, log(PRECIO_TE_IMP / PRECIO_TE_IMP_ANT), NA_real_),
  LOG_REL_VERDADERO = log(PRECIO_TE_VERDADERO / PRECIO_TE_IMP_ANT),
  LOG_F_CASCADA = ifelse(A_IMPUTAR == 1, log(F_IMP), NA_real_),
  LOG_F1 = log(F1), LOG_F2 = log(F2), LOG_F3 = log(F3), LOG_F4 = log(F4), LOG_F5 = log(F5),
  Q1, Q2, Q3, Q4, Q5,
  VAR_TC = log(COTIZACION / COTIZACION_ANT),
  N_IMPUT_ANT,
  LP_ANT = log(PRECIO_TE_IMP_ANT),
  LR_CALC = log(PRECIO_TE_IMP / PRECIO_TE_IMP_ANT)
)]
pares[, FILA := fila_id]
pares[, CAMBIA := ifelse(is.na(LOG_REL), NA_integer_, as.integer(abs(LOG_REL) > 1e-9))]
pares[, CAMBIA_VERDADERO := as.integer(abs(LOG_REL_VERDADERO) > 1e-9)]
pares[, NIVEL_PRECIO_REL := LP_ANT - stats::median(LP_ANT), by = .(ANIOMES, VARIEDAD, REGION)]
setorder(pares, FILA, ANIOMES)
for (k in 1:12) pares[, (sprintf("LOG_REL_LAG%02d", k)) := shift(LR_CALC, k), by = FILA]
pares[, MESES_SIN_CAMBIO := {
  r <- integer(.N); cnt <- 0L
  for (i in seq_len(.N)) { r[i] <- cnt; cnt <- if (abs(LR_CALC[i]) < 1e-9) cnt + 1L else 0L }
  r
}, by = FILA]
pares[, c("LP_ANT", "LR_CALC", "FILA") := NULL]
# Codigos como texto: los modelos los tratan como categorias
pares[, `:=`(REGION = as.character(REGION), TIPOEMP = as.character(TIPOEMP),
             GRUPO_TIPOEMP = as.character(GRUPO_TIPOEMP), NROINE = as.character(NROINE))]
setorder(pares, ANIOMES, NROINE, LOCAL, VARIEDAD)

# -- 10. Escritura ---------------------------------------------------------------
dir.create(cfg$salida, showWarnings = FALSE)
ult <- max(panel$ANIOMES)
saveRDS(panel, file.path(cfg$salida, "ipc_sintetico_panel.rds"), compress = "xz")
if (cfg$csv_completo) fwrite(panel, file.path(cfg$salida, "ipc_sintetico_panel.csv.gz"), sep = ";", dec = ",")
fwrite(panel[ANIOMES == ult], file.path(cfg$salida, sprintf("IPC_OUT_SINTETICO_%d.csv", ult)),
       sep = ";", dec = ",", quote = TRUE)
saveRDS(pares, file.path(cfg$salida, "ipc_sintetico_pares.rds"), compress = "xz")
if (cfg$csv_completo) fwrite(pares, file.path(cfg$salida, "ipc_sintetico_pares.csv.gz"), sep = ";", dec = ",")
file.copy(file.path("data-raw", "ipc", "clasificacion_ipc.csv"),
          file.path(cfg$salida, "clasificacion_ipc.csv"), overwrite = TRUE)

cat("Establecimientos:", nrow(est), "| Cadenas (NROINE):", n_cadenas,
    "| Filas por mes:", nrow(filas), "| Filas del panel:", nrow(panel), "\n")
print(file.info(list.files(cfg$salida, full.names = TRUE))[, "size", drop = FALSE])
