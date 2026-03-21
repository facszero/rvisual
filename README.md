# RVisual

> Addin de RStudio para usuarios de SPSS — Interfaz visual + generación automática de código R + Asistente IA contextual

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R version](https://img.shields.io/badge/R-%3E%3D%204.1-blue.svg)](https://www.r-project.org/)

---

## ¿Qué es RVisual?

RVisual es un addin para RStudio que permite a usuarios con experiencia en SPSS trabajar con datos en R sin necesidad de escribir código manualmente.

**Toda acción visual genera código R limpio, reproducible y editable.**

### Características principales

- **Panel Dataset** — Carga archivos (CSV, Excel, RDS, SAV) y detecta data.frames en memoria
- **Explorador** — Vista tabular, tipos de variables, estadísticas rápidas
- **Constructor Visual** — Filtros, selección, ordenamiento, agrupación, nuevas variables, recodificación y joins con mouse
- **Panel de Código R** — Código generado en tiempo real, insertable en scripts de RStudio
- **Asistente IA** — Copiloto contextual multi-proveedor (Anthropic, OpenAI, Gemini)
- **Configuración** — Gestión de API keys y preferencias de privacidad

---

## Instalación

```r
# Instalar desde GitHub
remotes::install_github("facszero/rvisual")
```

### Dependencias principales

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
# Desde la consola de RStudio
rvisual::launch_rvisual()

# O desde el menú: Addins → RVisual — Análisis Visual de Datos
```

---

## Configuración del Asistente IA

1. Abrí RVisual → pestaña **Configuración**
2. Seleccioná tu proveedor (Anthropic, OpenAI o Gemini)
3. Ingresá tu API key
4. Hacé clic en **Probar conexión** y luego **Guardar**

> La API key se guarda localmente. Por defecto, **no se envían filas de datos al proveedor** — solo el esquema (nombres y tipos de columnas).

---

## Arquitectura

```
rvisual/
├── R/
│   ├── addin.R              # Punto de entrada del addin
│   ├── ui.R                 # UI principal (bslib navbar)
│   ├── server.R             # Server orquestador
│   ├── core/
│   │   ├── operation_model.R   # Representación interna de operaciones
│   │   ├── code_generator.R    # Generación de código R (tidyverse)
│   │   ├── dataset_discovery.R # Descubrimiento y carga de datasets
│   │   └── rstudio_bridge.R    # Integración con RStudio API
│   ├── modules/
│   │   ├── mod_dataset.R    # Panel: Datos
│   │   ├── mod_explorer.R   # Panel: Explorador
│   │   ├── mod_builder.R    # Panel: Constructor visual
│   │   ├── mod_code.R       # Panel: Código R
│   │   ├── mod_ai.R         # Panel: Asistente IA
│   │   └── mod_config.R     # Panel: Configuración
│   ├── ai/
│   │   └── ai_interface.R   # Capa de integración multi-proveedor
│   └── utils/
│       └── config.R         # Configuración persistente + historial
├── inst/
│   ├── rstudio/addins.dcf   # Registro del addin en RStudio
│   └── www/custom.css       # Estilos de la interfaz
└── tests/testthat/
    └── test-core.R          # Tests unitarios del motor de operaciones
```

### Principio de diseño central

```
Operación visual → operation_model (lista R estructurada) → code_generator → código R limpio
```

Cada operación del usuario se almacena como un objeto tipado, no como texto concatenado. Esto permite regenerar código, soportar historial y facilitar auditoría.

---

## Roadmap

| Fase | Contenido | Estado |
|------|-----------|--------|
| 1 | Scaffolding + arquitectura base | ✅ En progreso |
| 2 | Gestión de datasets + exploración visual | 🔲 Pendiente |
| 3 | Constructor visual completo | 🔲 Pendiente |
| 4 | Generación de código + ejecución | 🔲 Pendiente |
| 5 | Integración IA multi-proveedor | 🔲 Pendiente |
| 6 | UX, testing, empaquetado | 🔲 Pendiente |

---

## Contribuir

Este proyecto está en fase inicial de desarrollo activo. Issues y PRs bienvenidos.

---

## Licencia

MIT © Fernando Cañete ([facszero](https://github.com/facszero))
