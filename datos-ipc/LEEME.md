# Datos sintéticos del IPC: imputación de precios con Machine Learning

Panel mensual **100% sintético** de precios del IPC (productos no especiales), con la misma estructura que la salida del cálculo (`IPC_OUT`). Sirve para practicar y evaluar la imputación de precios faltantes con Machine Learning, en RVisual o en cualquier otro entorno.

- **36 meses**, de noviembre de 2023 a octubre de 2026: 396.972 filas.
- **705 establecimientos** (572 NROINE), **479 variedades** de 331 productos y las 13 divisiones.
- **90 tipos de empresa**, con sus códigos reales, porque están asociados a los productos.
- 20% de los precios a imputar, imputados con la cascada F1–F6 igual que en producción.
- **Incluye el precio verdadero de cada faltante**, algo que con datos reales nunca se conoce.

Ningún establecimiento, canasta o precio corresponde a uno real (ver [Privacidad](#privacidad)).

## Archivos

| Archivo | Contenido |
| --- | --- |
| `ipc_sintetico_panel.rds` | Panel completo en formato `IPC_OUT` (una fila por establecimiento × variedad × mes) |
| `IPC_OUT_SINTETICO_202610.csv` | Último mes, mismo formato que el export del cálculo (`;` y coma decimal) |
| `ipc_sintetico_pares.rds` | Tabla lista para Machine Learning: objetivo, variables explicativas y verdad |
| `clasificacion_ipc.csv` | Etiquetas de división, grupo, clase, subclase y producto |

```r
panel <- readRDS("datos-ipc/ipc_sintetico_panel.rds")
pares <- readRDS("datos-ipc/ipc_sintetico_pares.rds")
ipc_out <- data.table::fread("datos-ipc/IPC_OUT_SINTETICO_202610.csv", sep = ";", dec = ",")
```

Los códigos de la clasificación se guardan como texto con sus ceros a la izquierda (`"01"`, `"0111120101"`); al leer el CSV con `fread` quedan numéricos, como en el cálculo.

## Panel (`ipc_sintetico_panel.rds` y el CSV del último mes)

Las columnas tienen el mismo nombre y significado que en el diccionario de variables del cálculo (`Variables_IPC`). Se incluyen las necesarias para la imputación:

| Grupo | Columnas |
| --- | --- |
| Período | `ANIOMES`, `ANIO`, `MES`, `SEMREP`, `SEMCALC`, `ESTADO` |
| Clasificación | `REGION`, `DIVISION`, `GRUPO`, `CLASE`, `SUBCLASE`, `SEISDIG`, `PRODUCTO`, `VARIEDAD`, `COMPONENTE` (0 en no especiales), `ESPECIAL` (siempre 2), `ESTACIONAL` |
| Informante | `NROINE`, `LOCAL`, `TIPOEMP` |
| Ponderadores | `W_I_R`, `W_I`, `GAMMA_I_R` |
| IVA y moneda | `IVA_D`, `IVA_C`, `IVA_DC`, `IVA_DD`, `IVA_T`, `FH`, `MONEDA`, `COTIZACION`, `COTIZACION_ANT` |
| Tamaños | `TR`, `TE`, `UNIMED` |
| Precios | `PRECIO_REL`, `PRECIO_REL_ANT`, `PRECIO_MN`, `PRECIO_MN_IMP`, `PRECIO_MN_IMP_ANT`, `PRECIO_TE`, `PRECIO_TE_IMP`, `PRECIO_TE_IMP_ANT`, `PRECIO_FIN`, `PRECIO_FIN_ANT`, `RELATIVO_VJ` |
| Faltantes e imputación | `RAZONFALTA`, `RAZONFALTA_ANT`, `A_IMPUTAR`, `A_IMPUTAR_ANT`, `F_IMP`, `QMIN`, `Q1`–`Q5`, `F1`–`F5`, `N_IMPUT`, `N_IMPUT_ANT`, `NIVELIMP` |
| **Solo sintético** | `PRECIO_TE_VERDADERO`: precio en tamaño estándar que habría tenido el informante, también cuando falta |

Las relaciones entre columnas son las del cálculo: `PRECIO_MN = PRECIO_REL × COTIZACION` si `MONEDA = 2`; `PRECIO_TE_IMP = PRECIO_MN_IMP × TE / TR`; en los imputados, `PRECIO_TE_IMP = PRECIO_TE_IMP_ANT × F_IMP`; `PRECIO_FIN = PRECIO_TE_IMP × (1 − (IVA_D·IVA_DD + IVA_C·IVA_DC) / (1 + IVA_T))`. El precio imputado pasa a t−1 en el mes siguiente, como en producción.

No se incluyen los índices agregados (`INDICE_*`, `VAR_*`, `INC_*`) ni los productos especiales.

## Pares para Machine Learning (`ipc_sintetico_pares.rds`)

Una fila por establecimiento × variedad × mes, con la variable objetivo de la propuesta (`y = ln(P(t) / P(t−1))`) y las variables candidatas:

| Columna | Descripción |
| --- | --- |
| `LOG_REL` | **Objetivo.** Log-relativo de los pares observados en t y en t−1; vacío en los demás |
| `LOG_REL_VERDADERO` | Log-relativo verdadero (precio verdadero en t sobre el precio del cálculo en t−1), para todas las filas |
| `LOG_F_CASCADA` | Log del factor que aplicó la cascada a los imputados: la referencia contra la que comparar un modelo |
| `CAMBIA`, `CAMBIA_VERDADERO` | 1 si el precio cambió (observado / verdadero): para el modelo en dos etapas |
| `A_IMPUTAR`, `A_IMPUTAR_ANT`, `NIVELIMP` | Faltante en t y t−1, y nivel de la cascada usado |
| `LOG_F1`–`LOG_F5`, `Q1`–`Q5` | Factores y coberturas de la cascada en el mes: "la cascada como variable" |
| `LOG_REL_LAG01`–`LOG_REL_LAG12` | Log-relativos del cálculo de los 12 meses anteriores |
| `MESES_SIN_CAMBIO`, `N_IMPUT_ANT` | Meses seguidos sin cambio de precio y meses seguidos imputado |
| `NIVEL_PRECIO_REL` | Precio del local en t−1 frente a la mediana de su variedad y región (log) |
| `VAR_TC` | Variación del tipo de cambio (log) |
| `MES`, `REGION`, `TIPOEMP`, `GRUPO_TIPOEMP`, `MONEDA`, `ESTACIONAL`, `IVA_T` | Calendario, informante y producto |
| `DIVISION` … `VARIEDAD`, `DESC_DIVISION`, `DESC_PRODUCTO` | Clasificación con etiquetas |

**Entrenar** con las filas donde `LOG_REL` existe; **evaluar** sobre las filas con `A_IMPUTAR = 1`, comparando la predicción contra `LOG_REL_VERDADERO` y la cascada (`LOG_F_CASCADA`) contra lo mismo.

Error de la cascada frente a la verdad en los 80.023 precios imputados del panel: RMSE de 0,21 en log y sesgo de −0,003. Por nivel: 0,27 en el nivel 1, 0,14 en el 2, 0,17 en el 3 y 0,22 en el 4.

### En RVisual

1. Cargar `ipc_sintetico_pares.rds` en el panel Dataset. Para empezar, conviene filtrar en el Constructor una división y unos meses (por ejemplo `DIVISION == "01"` y `ANIOMES >= 202511`, unas 63.000 filas).
2. En **Modelado (ML)**: objetivo `LOG_REL`; como predictores, por ejemplo `LOG_F2`, `LOG_F3`, `Q2`, `NIVEL_PRECIO_REL`, `MESES_SIN_CAMBIO`, `LOG_REL_LAG01`, `LOG_REL_LAG02`, `LOG_REL_LAG12`, `N_IMPUT_ANT`, `TIPOEMP`, `REGION`, `MES` y `VAR_TC`.
3. El diagnóstico marca **muchos ceros** (los precios rígidos, alrededor de 60%) y las filas sin objetivo (las que hay que imputar).

Dos funcionalidades de la propuesta todavía no están en el panel y llegan en fases siguientes: el **modelo en dos etapas** y la **validación temporal**. Mientras tanto, la validación cruzada del panel mezcla meses, que sirve para practicar pero no para la evaluación final.

## Cómo se generó

El generador (`data-raw/ipc/02_generar_ipc.R`) se calibró (`01_calibrar_ipc.R`) con **parámetros agregados** de una salida del cálculo de octubre de 2026 con precios y tamaños alterados respecto de producción. Esa salida no se distribuye; los parámetros están en `data-raw/ipc/parametros_ipc.rds`.

| Mecanismo | Cómo se simula |
| --- | --- |
| Establecimientos | Tipos de empresa reales; cantidad por región proporcional a la real |
| Cadenas | Distribución real de locales por NROINE según grupo de tipo de empresa |
| Canastas | Cada variedad con su probabilidad dentro del tipo de empresa, más una perturbación (descartes y agregados del mismo grupo) |
| Nivel de precios | Mediana por variedad y región, efecto de cadena (35% de la dispersión) y dispersión por variedad |
| Rigidez | Probabilidad de cambio por división; en los productos en dólares se mide en dólares y el tipo de cambio se simula aparte |
| Tamaño de los cambios | Distribución empírica por división, con componente común por variedad-región (25%) y reversión parcial hacia el nivel propio |
| Coordinación de cadenas | 79% de los locales de una cadena siguen la decisión de la cadena |
| Inflación | 4,55% anual (variación interanual publicada) |
| Faltantes | Dos capas: establecimiento entero sin respuesta (persistencia de 86% a 91% según tamaño) y filas sueltas (Markov), más un supuesto de faltante no aleatorio |
| Estacionales | Las 19 variedades estacionales son de invierno o de verano; fuera de temporada faltan en todos los comercios |
| Cálculo | Cascada F1–F6 con QMIN = 1/3 (ver abajo) |

### Comparación con la salida de calibración

| Métrica | Export (oct. 2026) | Sintético (oct. 2026) | Sintético (36 meses) |
| --- | --- | --- | --- |
| Precios a imputar | 18,5% | 20,0% | 20,2% |
| P(falta en t \| no faltó en t−1) | 0,056 | 0,078 | 0,058 |
| P(falta en t \| faltó en t−1) | 0,748 | 0,747 | 0,790 |
| Relativos observados sin cambio | 67,9% | 67,5% | 67,6% |
| Imputados en nivel 1 / 2 / 4 | 44% / 37% / 18% | 37% / 46% / 15% | 40% / 47% / 10% |
| Dispersión del log-precio dentro de variedad-región | 0,38 | 0,42 | 0,42 |
| Coordinación de cadenas | 0,79 | 0,78 | — |

La proporción sin cambio por división también coincide (por ejemplo: 61% contra 62% en la división 01, 28% contra 27% en la 08), con una excepción: la **07 (transporte)** cambia de más (36% sin cambio contra 58%). La perturbación de canastas suma algunas variedades cotizadas en dólares (3,9% de las filas contra 2,4%), y en pesos esas cambian todos los meses con el tipo de cambio. La inflación verdadera simulada ronda el 4% al 5% anual, según la semilla.

### La cascada reproduce la de producción

Aplicada al export real, la emulación de la cascada elige **el mismo nivel en el 100% de los 8.603 precios imputados** y calcula Q1–Q5 idénticos. El factor coincide exacto en el 83% de los casos. En los niveles 4 y 5 es una aproximación, con un error relativo medio de 0,04%, porque la ponderación exacta usa información que no está en la salida del cálculo.

| Nivel | Celda | Factor | Cobertura Q |
| --- | --- | --- | --- |
| 1 | Variedad × tipo de empresa × región (solo divisiones 01 y 02) | Media geométrica de los relativos de los pares observados en t y t−1 | Pares observados / filas |
| 2 | Variedad × región | Ídem | Ídem |
| 3 | Producto × región | Ídem | Ídem |
| 4 | Subclase × región | Media geométrica de F3 ponderada por `W_I_R` | Peso de los productos con Q3 ≥ QMIN / peso de la subclase |
| 5 | Clase × región | Media geométrica de F4 ponderada | Análogo, con las subclases |
| 6 | Sin datos | 1 | — |

## Privacidad

- **NROINE** son números de 9 dígitos desde 900.000.001, fuera del rango de los reales; **LOCAL** se numera 1, 2, … dentro de cada NROINE.
- **Canastas:** ninguna canasta sintética (de 5 o más variedades) coincide con la canasta de un establecimiento real único.
- **Precios:** simulados desde cero; ningún precio sale de un registro real.
- **TIPOEMP** conserva los códigos reales, a pedido, porque están asociados a los productos y hacen el dataset reconocible para quien conoce el IPC.
- Los parámetros de calibración son agregados: proporciones, cuantiles, medianas por variedad y probabilidades por tipo de empresa. No contienen NROINE, LOCAL ni filas individuales.

## Regenerar

Desde la raíz del repositorio:

```bash
Rscript data-raw/ipc/02_generar_ipc.R             # 700 establecimientos (estos archivos)
Rscript data-raw/ipc/02_generar_ipc.R 3400        # tamaño real aproximado
Rscript data-raw/ipc/02_generar_ipc.R 700 --csv   # además, CSV del panel y de los pares
```

Para recalibrar con otra salida del cálculo (por ejemplo, de producción y dentro de INE):

```bash
Rscript data-raw/ipc/01_calibrar_ipc.R ruta/al/IPC_OUT.csv
```

Los supuestos que un solo mes no permite medir están al final de `01_calibrar_ipc.R`: inflación, amplitud estacional, volatilidad del tipo de cambio, faltante no aleatorio y reversión.

## Límites

- **Calibrado con un solo mes.** La persistencia de faltantes y de cambios, la estacionalidad y la autocorrelación de los rezagos son supuestos o extrapolaciones. Para calibrarlas bien hace falta correr `01_calibrar_ipc.R`, adaptado, sobre 12 a 36 meses.
- **Sin altas, bajas ni sustituciones:** el panel es balanceado (cada establecimiento-variedad está los 36 meses).
- **Sin productos especiales.**
- **F4 y F5** son una aproximación del cálculo de producción (ver arriba).
