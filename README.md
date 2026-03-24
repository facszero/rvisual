# RVisual

> Addin de RStudio para usuarios de SPSS — Interfaz visual + generación automática de código R + Asistente IA contextual

[![R CMD check](https://github.com/facszero/rvisual/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/facszero/rvisual/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R version](https://img.shields.io/badge/R-%3E%3D%204.1-blue.svg)](https://www.r-project.org/)
[![R-universe](https://facszero.r-universe.dev/badges/rvisual)](https://facszero.r-universe.dev/rvisual)

---

## ¿Qué es RVisual?

RVisual es un addin para RStudio que permite a usuarios con experiencia en SPSS trabajar con datos en R sin necesidad de escribir código manualmente.

**Toda acción visual genera código R limpio, reproducible y editable.**

### Características principales

* **Panel Dataset** — Carga archivos (CSV, Excel, RDS, SAV hasta 50 MB) y detecta data.frames en memoria
* **Explorador** — Vista tabular, tipos de variables, estadísticas rápidas
* **Constructor Visual** — Filtros, selección, ordenamiento, agrupación, nuevas variables, recodificación y joins con mouse
* **Panel de Código R** — Código generado en tiempo real, ejecutable con resultado inline
* **Asistente IA** — Copiloto contextual multi-proveedor (Anthropic, OpenAI, Gemini)
* **Configuración** — Gestión de API keys y preferencias de privacidad

---

## Instalación

### Desde R-universe (recomendado)

```r
install.packages("rvisual", repos = "https://facszero.r-universe.dev")
```

### Desde GitHub

```r
# install.packages("remotes")
remotes::install_github("facszero/rvisual")
```

### Dependencias

```r
install.packages(c(
  "shiny", "miniUI", "bslib", "DT", "rstudioapi",
  "dplyr", "tidyr", "readr", "readxl",
  "httr2", "jsonlite", "glue", "rlang"
))
```

---

## Uso

```r
library(rvisual)

# Abrir en panel Viewer de RStudio
launch_rvisual()

# Abrir en browser (necesario para el Asistente IA en redes con proxy)
launch_rvisual(browser = TRUE)
```

O desde el menú: **Addins → RVisual — Análisis Visual de Datos**

---

## Configuración del Asistente IA

1. Abrí RVisual → pestaña **Configuración**
2. Seleccioná tu proveedor (Anthropic, OpenAI o Gemini)
3. Ingresá tu API key
4. Hacé clic en **Probar conexión** y luego **Guardar**

> La API key se guarda localmente. Por defecto, **no se envían filas de datos al proveedor** — solo el esquema (nombres y tipos de columnas).

### Proxy corporativo

Si trabajás en una red con proxy, configuralo antes de lanzar:

```r
Sys.setenv(http_proxy  = "http://proxy.empresa.com:3128")
Sys.setenv(https_proxy = "http://proxy.empresa.com:3128")
launch_rvisual(browser = TRUE)
```

---

## Arquitectura

```
rvisual/
├── R/
│   ├── addin.R              # Punto de entrada — launch_rvisual()
│   ├── ui.R                 # UI principal (bslib navbar)
│   ├── server.R             # Server orquestador
│   ├── mod_dataset.R        # Panel: Datos
│   ├── mod_explorer.R       # Panel: Explorador
│   ├── mod_builder.R        # Panel: Constructor visual
│   ├── mod_code.R           # Panel: Código R
│   ├── mod_ai.R             # Panel: Asistente IA
│   ├── mod_config.R         # Panel: Configuración
│   ├── ai_interface.R       # Integración multi-proveedor IA
│   ├── operation_model.R    # Modelo de operaciones visuales
│   ├── code_generator.R     # Generación de código tidyverse
│   ├── dataset_discovery.R  # Metadatos y carga de archivos
│   ├── rstudio_bridge.R     # Integración con RStudio API
│   └── config.R             # Configuración persistente
└── tests/testthat/
    └── test-core.R          # 47 tests unitarios
```

### Principio de diseño central

```
Acción visual → operation_model → code_generator → código R limpio
```

---

## Roadmap

| Fase | Contenido | Estado |
| --- | --- | --- |
| 1 | Scaffolding + arquitectura base | ✅ Completo |
| 2 | Gestión de datasets + exploración visual | ✅ Completo |
| 3 | Constructor visual completo | ✅ Completo |
| 4 | Generación de código + ejecución | ✅ Completo |
| 5 | Integración IA multi-proveedor | ✅ Completo |
| 6 | Guardar archivos + exportar resultados | ✅ Completo |
| 7 | UX, testing completo, CRAN | 🔲 Pendiente |

---

## Contribuir

Issues y PRs bienvenidos en [github.com/facszero/rvisual](https://github.com/facszero/rvisual).

---

## Licencia

MIT © Fernando Cañete ([facszero](https://github.com/facszero))
