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

## Mejoras y correcciones (desarrollo post-CRAN submission)

### Nuevas funcionalidades

* **Conexión a bases de datos** (Fase 7b) — Nueva pestaña "Base de datos" en el
  panel Dataset. Soporte para PostgreSQL, MySQL/MariaDB, SQL Server, Oracle,
  SQLite e IBM DB2. Configuración por motor persistida entre sesiones
  (sin contraseña). Soporte para Oracle Service Name, SID y strings TNS completos.

* **Exportar resultado a CSV y Excel** (Fase 7a) — Botones "Resultado → CSV" y
  "Resultado → Excel" en el panel Código R. Exporta el data.frame resultante
  de ejecutar el código generado.

* **Exportar dataset activo** — Botones CSV, Excel y RDS en el panel Dataset.

### Correcciones

* `safe_col()` — nombres de columna con espacios, puntos, guiones o que
  empiezan con número ahora se envuelven en backticks automáticamente
  (ej: `Nro. INE` → `` `Nro. INE` ``).

* SQL por motor en carga de tablas — cada motor usa su dialecto correcto:
  Oracle (`WHERE ROWNUM <=`), SQL Server (`SELECT TOP`),
  DB2 (`FETCH FIRST n ROWS ONLY`), otros (`LIMIT`).
