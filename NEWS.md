# rvisual 0.1.0

## Primera versión pública

### Características principales

* **Panel Dataset** — Carga de archivos CSV, Excel, RDS y SAV (hasta 50 MB).
  Detección automática de data.frames en el entorno global.
* **Panel Explorador** — Vista tabular paginada con estadísticas por variable.
* **Panel Constructor** — 8 operaciones visuales que generan código tidyverse:
  select, filter, arrange, group_summarise, mutate, rename, recode, join.
* **Panel Código R** — Código generado en tiempo real, ejecutable con
  visualización del resultado inline.
* **Asistente IA** — Copiloto contextual multi-proveedor (OpenAI, Anthropic,
  Gemini) con soporte de proxy corporativo.
* **Panel Configuración** — Gestión de API keys y preferencias de privacidad.

### Notas técnicas

* Requiere R >= 4.1.0 (pipe nativo `|>`)
* Compatible con redes corporativas con proxy HTTP/HTTPS
* El Asistente IA requiere `launch_rvisual(browser = TRUE)` en entornos
  donde el panel Viewer de RStudio bloquea conexiones HTTP salientes
