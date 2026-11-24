# rvisual (en desarrollo)

## Nuevo panel: Modelado (ML)

* Asistente de cinco pasos para aprendizaje supervisado sobre datos
  tabulares: objetivo, diagnóstico, partición y modelos, comparación por
  validación cruzada y modelo final. Regresión, clasificación binaria y
  multiclase. Ver `vignette("modelado", package = "rvisual")`.
* **Diagnóstico** con reglas explícitas: identificadores, alta
  cardinalidad, NA, pocas filas, muchos términos, clases desbalanceadas,
  muchos ceros, asimetría y **posible fuga de información** (las variables
  con fuga se excluyen por defecto).
* **Modelos:** línea base, regresión lineal y logística, elastic net
  (`glmnet`), árbol (`rpart`), Random Forest (`ranger`) y gradient boosting
  (`xgboost` >= 3.2.1.1). Los paquetes de ML están en `Suggests`: la
  instalación de RVisual no cambia.
* **Script reproducible:** todo el modelado se entrega como un script R
  autocontenido (carga, operaciones del Constructor, partición, validación
  cruzada y modelo final). Las métricas del panel son exactamente las que
  produce el script.
* El modelo final se puede enviar al panel Código R, ejecutar en la
  consola o descargar (`.rds`), junto con el script (`.R`).
* **Aplicar a datos nuevos (paso 6):** predice con el modelo final sobre
  otro conjunto de filas (otro dataset, o el mismo con otro filtro, como
  `A_IMPUTAR == 1`) y lo guarda como un dataset nuevo en la pestaña
  Dataset, sin cambiar el activo. Los datos se preparan igual que el
  entrenamiento (tipos, niveles de factores, imputación con la referencia
  del entrenamiento) y el panel informa las filas sin predicción y las
  categorías no vistas. El script suma la sección 9 con la misma
  aplicación.

## Directorio de trabajo

* La pestaña Dataset muestra el directorio de trabajo y permite cambiarlo
  con el selector de carpetas de RStudio (o escribiendo la ruta). Si el
  dataset activo vino de un archivo, avisa si ese archivo está en el
  directorio: el código generado lo carga por su nombre.

## Datos de ejemplo

* Nuevo juego de datos sintético `ech_sintetica` (2.213 personas, 17
  variables) en `inst/extdata`, en CSV, Excel, RDS, SPSS y SQLite, más una
  tabla de departamentos para probar uniones. Diccionario y ejercicios por
  panel en `inst/extdata/LEEME.md`.

## Correcciones

* **Filtrar y recodificar texto que parece número:** un filtro como
  `DIVISION == "01"` se escribía como `DIVISION == 01` y no devolvía
  filas (lo mismo con variedades, productos y cualquier código con ceros a
  la izquierda). Ahora el tipo de la columna decide si el valor va entre
  comillas, en la vista previa y en el código generado.
* **Unir tablas (join):** el diálogo solo ofrecía los data.frames del
  entorno global de R, así que un dataset cargado en RVisual no se podía
  usar como segunda tabla. Ahora aparecen los dos grupos. Además:
  * se verifican los tipos de las claves antes de aplicar; si una es
    número y la otra texto con solo dígitos, se convierte la de texto a
    número, y cualquier otra combinación se informa sin aplicar el join;
  * se pueden elegir las columnas a traer de la tabla derecha, con aviso de
    las que quedarían duplicadas (`.x` / `.y`);
  * el código generado (y el script de Modelado) incluye la carga del
    segundo dataset.
* **Panel de base de datos:** elegir SQLite como motor cortaba la sesión
  (`subscript out of bounds`).
* **Panel de base de datos:** al cerrar la app aparecía el error
  "Operation not allowed without an active reactive context" y una
  conexión abierta quedaba sin cerrar.
* **Código generado en Windows:** las rutas con barras invertidas
  (`C:\Users\...`) hacían que el script no se pudiera leer
  ("'\U' used without hex digits"). Afectaba a los datasets cargados desde
  SQLite.

# rvisual 0.1.1

## Correcciones (Round 2 de pruebas)

* **`summarise` sin `group_by`** — La operación "Agrupar y resumir" ahora
  permite no seleccionar columnas de agrupación, calculando estadísticas
  globales (ej: media de toda la columna). Antes se bloqueaba silenciosamente.

* **Eliminación de operación intermedia** — Al eliminar una operación del
  stack con el botón X, las operaciones siguientes ya no se eliminaban en
  cascada. El bug era causado por `once = TRUE` en observers Shiny acumulados;
  se reemplazó por un patrón `reactiveVal` centralizado.

* **Cambio de dataset con operaciones activas** — Al cambiar el dataset activo
  mientras hay operaciones en el Constructor, ahora se muestra un modal de
  confirmación informando cuántas operaciones se perderán. Si el usuario
  confirma, el stack se limpia y se activa el nuevo dataset.

* **Código generado limpio para agregación global** — `code_group_summarise()`
  ahora genera `dplyr::summarise()` sin `dplyr::group_by()` cuando no hay
  columnas de agrupación, produciendo código más idiomático.

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
