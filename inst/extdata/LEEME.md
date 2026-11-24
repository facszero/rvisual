# Juego de datos de ejemplo: `ech_sintetica`

Datos **100% sintéticos**, inspirados en la estructura de una encuesta de hogares (personas dentro de hogares, departamentos, condición de actividad, ingresos). No provienen de microdatos reales y no reproducen estimaciones oficiales: las relaciones entre variables son plausibles pero inventadas. Se pueden usar y compartir libremente para probar RVisual.

- 2.213 personas en 800 hogares, 17 variables.
- Mismo contenido en cinco formatos, para probar cada forma de carga.
- Se regenera con `data-raw/generar_ech_sintetica.R` (semilla fija).

## Archivos

| Archivo | Contenido | Para probar |
| --- | --- | --- |
| `ech_sintetica.csv` | Personas (UTF-8, separador coma) | Carga de CSV |
| `ech_sintetica.xlsx` | Hojas `ech_sintetica` y `departamentos` | Carga de Excel |
| `ech_sintetica.rds` | Personas (tipos de R conservados, fecha como `Date`) | Carga de RDS |
| `ech_sintetica.sav` | Personas | Carga de SPSS |
| `ech_sintetica.sqlite` | Tablas `personas` y `departamentos` | Conexión a base de datos (SQLite) |
| `departamentos.csv` | 19 departamentos con nombre y región | Unir tablas en el Constructor |

Desde R, la ruta de cualquier archivo se obtiene con:

```r
system.file("extdata", "ech_sintetica.csv", package = "rvisual")
```

También se pueden descargar desde GitHub, en la carpeta `inst/extdata` del repositorio.

## Diccionario de variables (`ech_sintetica`)

| Variable | Tipo | Descripción |
| --- | --- | --- |
| `id_persona` | entero | Identificador único de persona (1 a 2.213) |
| `id_hogar` | entero | Identificador del hogar (800 hogares) |
| `fecha_entrevista` | fecha | Fecha de la entrevista, durante 2025 |
| `departamento` | entero | Código de departamento (1 a 19); se une con `departamentos.csv` |
| `localidad` | texto | Código de localidad (`LOC_001` … `LOC_085`) |
| `estrato` | entero | Estrato de muestreo (1 a 5) |
| `peso` | entero | Peso de expansión (153 a 598) |
| `sexo` | texto | `Hombre` / `Mujer` |
| `edad` | entero | Edad en años (14 a 90) |
| `anios_educ` | entero | Años de educación aprobados (0 a 20) |
| `nivel_educ` | texto | `Primaria`, `Ciclo básico`, `Bachillerato`, `Terciaria` (25 NA) |
| `condicion_actividad` | texto | `Ocupado` (54,0%), `Desocupado` (6,4%), `Inactivo` (39,6%) |
| `ocupado` | texto | `Si` / `No` (deriva de `condicion_actividad`) |
| `horas_trabajadas` | entero | Horas semanales; 0 si no está ocupado (37 NA entre ocupados) |
| `rama_actividad` | texto | Rama de actividad; `No aplica` si no está ocupado |
| `aporta_bps` | texto | `Si` / `No`: aporta a la seguridad social (solo puede ser `Si` si está ocupado) |
| `ingreso_laboral` | numérico | Ingreso laboral mensual en pesos; 0 si no está ocupado (76 NA entre ocupados) |

## Ejercicios sugeridos por panel

**Dataset y Explorador**
- Cargar el mismo dataset desde CSV, Excel, RDS y SPSS, y comprobar que en el Explorador se ve igual.
- Revisar en el Explorador los NA de `ingreso_laboral`, `horas_trabajadas` y `nivel_educ`.

**Constructor**
- Filtrar `ocupado == "Si"` y resumir el ingreso medio por `sexo` o por `nivel_educ`.
- Recodificar `sexo` (`Hombre` → `H`, `Mujer` → `M`).
- Unir con `departamentos.csv` por `departamento` y resumir el ingreso medio por `region`.

**Base de datos**
- Conectarse a `ech_sintetica.sqlite` con el tipo SQLite y leer la tabla `personas` o `departamentos`.

**Modelado (ML)**

| Tarea | Objetivo | Predictores sugeridos | Qué debería mostrar el diagnóstico |
| --- | --- | --- | --- |
| Clasificación binaria | `ocupado` (positiva `Si`) | `edad`, `sexo`, `anios_educ`, `departamento` | Comparar con la línea base; AUC claramente mayor a 0,5 |
| Misma tarea, a propósito mal planteada | `ocupado` | agregar `id_persona`, `localidad`, `condicion_actividad`, `ingreso_laboral` | Identificador y alta cardinalidad excluidos; **posible fuga** por `condicion_actividad` e `ingreso_laboral` (también excluidas por defecto) |
| Regresión | `ingreso_laboral` | `edad`, `sexo`, `anios_educ`, `horas_trabajadas` | **Muchos ceros** (los no ocupados) y filas sin objetivo |
| Regresión sobre ocupados | `ingreso_laboral`, con el filtro `ocupado == "Si"` del Constructor | `anios_educ`, `horas_trabajadas`, `sexo`, `edad` | El modelo lineal supera claramente a la línea base |
| Multiclase | `condicion_actividad` | `edad`, `sexo`, `anios_educ` | **Clases desbalanceadas** (`Desocupado` 6,4%) |

Para pensar después de la clasificación binaria: si se deja `aporta_bps` entre los predictores, aparece primera en la importancia de variables. No es casualidad: en estos datos, aportar a la seguridad social implica estar ocupado. Es una fuga **parcial** que el diagnóstico no marca (quienes no aportan pueden estar ocupados o no), y la pregunta del analista es la misma: ¿ese dato estará disponible en el momento de predecir?
