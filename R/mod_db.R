#' M\u00f3dulo: Conexi\u00f3n a Base de Datos
#'
#' Permite conectarse a PostgreSQL, MySQL, SQL Server, Oracle, SQLite y DB2
#' y cargar tablas como datasets activos en RVisual.

# ── Helpers internos ──────────────────────────────────────────────────────

#' @keywords internal
db_drivers <- function() {
  list(
    postgresql = list(label = "PostgreSQL",  pkg = "RPostgres",  drv = "RPostgres::Postgres()",
                      port = 5432,  needs_db = TRUE,  needs_host = TRUE),
    mysql      = list(label = "MySQL/MariaDB", pkg = "RMySQL",   drv = "RMySQL::MySQL()",
                      port = 3306,  needs_db = TRUE,  needs_host = TRUE),
    sqlserver  = list(label = "SQL Server",  pkg = "odbc",       drv = "odbc::odbc()",
                      port = 1433,  needs_db = TRUE,  needs_host = TRUE),
    oracle     = list(label = "Oracle",      pkg = "ROracle",    drv = "ROracle::Oracle()",
                      port = 1521,  needs_db = FALSE, needs_host = TRUE),
    sqlite     = list(label = "SQLite",      pkg = "RSQLite",    drv = "RSQLite::SQLite()",
                      port = NA,    needs_db = FALSE, needs_host = FALSE),
    db2        = list(label = "IBM DB2",     pkg = "odbc",       drv = "odbc::odbc()",
                      port = 50000, needs_db = TRUE,  needs_host = TRUE)
  )
}

#' @keywords internal
db_connect <- function(tipo, host, port, dbname, user, password, filepath = NULL,
                       driver_name = NULL) {
  drivers <- db_drivers()
  cfg     <- drivers[[tipo]]

  # Verificar que el paquete necesario est\u00e1 disponible
  pkg <- cfg$pkg
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Paquete '", pkg, "' no instalado. Ejecut\u00e1: install.packages('", pkg, "')"))
  }

  con <- switch(tipo,
    postgresql = DBI::dbConnect(
      RPostgres::Postgres(),
      host     = host,
      port     = as.integer(port),
      dbname   = dbname,
      user     = user,
      password = password
    ),
    mysql = DBI::dbConnect(
      RMySQL::MySQL(),
      host     = host,
      port     = as.integer(port),
      dbname   = dbname,
      user     = user,
      password = password
    ),
    sqlserver = DBI::dbConnect(
      odbc::odbc(),
      Driver   = if (!is.null(driver_name) && nchar(driver_name) > 0)
                   driver_name else "ODBC Driver 17 for SQL Server",
      Server   = paste0(host, ",", port),
      Database = dbname,
      UID      = user,
      PWD      = password
    ),
    oracle = DBI::dbConnect(
      ROracle::Oracle(),
      username = user,
      password = password,
      dbname   = paste0(host, ":", port, "/", dbname)
    ),
    sqlite = DBI::dbConnect(
      RSQLite::SQLite(),
      dbname = filepath
    ),
    db2 = DBI::dbConnect(
      odbc::odbc(),
      Driver   = if (!is.null(driver_name) && nchar(driver_name) > 0)
                   driver_name else "IBM DB2 ODBC DRIVER",
      Database = dbname,
      Hostname = host,
      Port     = as.integer(port),
      UID      = user,
      PWD      = password
    ),
    stop(paste0("Tipo de BD no soportado: ", tipo))
  )
  con
}

# ── UI ────────────────────────────────────────────────────────────────────

#' @param id ID del m\u00f3dulo Shiny.
mod_db_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h5(shiny::icon("database"), " Conectar a base de datos",
              style = "margin-top:0"),

    # Selector de tipo de BD
    shiny::selectInput(ns("tipo"), "Motor",
      choices = c(
        "PostgreSQL"    = "postgresql",
        "MySQL/MariaDB" = "mysql",
        "SQL Server"    = "sqlserver",
        "Oracle"        = "oracle",
        "SQLite"        = "sqlite",
        "IBM DB2"       = "db2"
      ),
      selected = "postgresql"
    ),

    # SQLite: solo selector de archivo
    shiny::conditionalPanel(
      condition = paste0("input['", ns("tipo"), "'] == 'sqlite'"),
      shiny::fileInput(ns("sqlite_file"), "Archivo .db / .sqlite",
        accept = c(".db", ".sqlite", ".sqlite3"),
        buttonLabel = "Seleccionar\u2026"
      )
    ),

    # Otros: formulario de conexi\u00f3n
    shiny::conditionalPanel(
      condition = paste0("input['", ns("tipo"), "'] != 'sqlite'"),
      shiny::fluidRow(
        shiny::column(8,
          shiny::textInput(ns("host"), "Host / Servidor", value = "localhost")),
        shiny::column(4,
          shiny::numericInput(ns("port"), "Puerto", value = 5432, min = 1, max = 65535))
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("tipo"), "'] != 'oracle'"),
        shiny::textInput(ns("dbname"), "Base de datos / Schema", placeholder = "mi_base")
      ),
      shiny::fluidRow(
        shiny::column(6,
          shiny::textInput(ns("user"), "Usuario")),
        shiny::column(6,
          shiny::passwordInput(ns("password"), "Contrase\u00f1a"))
      ),
      # Driver ODBC (solo SQL Server y DB2)
      shiny::conditionalPanel(
        condition = paste0(
          "input['", ns("tipo"), "'] == 'sqlserver' || ",
          "input['", ns("tipo"), "'] == 'db2'"
        ),
        shiny::textInput(ns("driver_name"), "Driver ODBC (opcional)",
          placeholder = "dejar vac\u00edo para usar el predeterminado")
      )
    ),

    shiny::actionButton(ns("btn_conectar"), "Conectar",
      icon = shiny::icon("plug"), class = "btn-primary btn-sm"),
    shiny::div(class = "ds-hint mt-1",
               shiny::textOutput(ns("msg_conexion"))),

    shiny::hr(),

    # Panel de exploraci\u00f3n (visible solo si hay conexi\u00f3n activa)
    shiny::uiOutput(ns("panel_explorar"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────

#' @param id ID del m\u00f3dulo Shiny.
#' @param active_dataset reactiveVal para el data.frame activo.
#' @param active_name reactiveVal para el nombre del dataset activo.
#' @param datasets_rv reactiveValues con la lista de datasets cargados.
mod_db_server <- function(id, active_dataset, active_name, datasets_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    con_activa  <- shiny::reactiveVal(NULL)   # conexi\u00f3n DBI activa
    tablas_list <- shiny::reactiveVal(NULL)   # vector de tablas disponibles

    # ── Actualizar puerto al cambiar motor ────────────────────────────────
    shiny::observeEvent(input$tipo, {
      puertos <- c(postgresql = 5432, mysql = 3306, sqlserver = 1433,
                   oracle = 1521, db2 = 50000)
      p <- puertos[[input$tipo]]
      if (!is.na(p)) shiny::updateNumericInput(session, "port", value = p)
    })

    # ── Conectar ──────────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_conectar, {
      # Cerrar conexi\u00f3n anterior si existe
      if (!is.null(con_activa())) {
        tryCatch(DBI::dbDisconnect(con_activa()), error = function(e) NULL)
        con_activa(NULL)
        tablas_list(NULL)
      }

      shiny::withProgress(message = "Conectando\u2026", value = 0.5, {
        result <- tryCatch({
          filepath <- if (input$tipo == "sqlite" && !is.null(input$sqlite_file))
                        input$sqlite_file$datapath else NULL

          con <- db_connect(
            tipo        = input$tipo,
            host        = input$host,
            port        = input$port,
            dbname      = input$dbname,
            user        = input$user,
            password    = input$password,
            filepath    = filepath,
            driver_name = input$driver_name
          )
          list(ok = TRUE, con = con)
        }, error = function(e) {
          list(ok = FALSE, msg = e$message)
        })

        if (result$ok) {
          con_activa(result$con)
          tablas <- tryCatch(DBI::dbListTables(result$con), error = function(e) character(0))
          tablas_list(tablas)
          output$msg_conexion <- shiny::renderText(
            paste0("\u2713 Conectado. ", length(tablas), " tabla(s) disponible(s).")
          )
          shiny::showNotification(
            paste0("Conexi\u00f3n exitosa \u2014 ", length(tablas), " tablas"),
            type = "message", duration = 3)
        } else {
          output$msg_conexion <- shiny::renderText(
            paste0("\u2717 Error: ", result$msg)
          )
          shiny::showNotification(
            paste0("Error de conexi\u00f3n: ", result$msg),
            type = "error", duration = 8)
        }
      })
    })

    # ── Panel de exploraci\u00f3n ─────────────────────────────────────────────
    output$panel_explorar <- shiny::renderUI({
      shiny::req(con_activa(), tablas_list())
      tablas <- tablas_list()
      if (length(tablas) == 0) {
        return(shiny::p(class = "ds-hint", "No se encontraron tablas."))
      }

      shiny::tagList(
        shiny::h6(shiny::icon("table"), " Seleccionar tabla"),
        shiny::selectInput(ns("tabla_sel"), NULL,
          choices = c("-- elegir --" = "", tablas),
          selected = ""
        ),
        shiny::fluidRow(
          shiny::column(7,
            shiny::textInput(ns("nombre_tabla_ds"),
              "Nombre del dataset", placeholder = "ej: clientes")
          ),
          shiny::column(5,
            shiny::numericInput(ns("limite_filas"),
              "M\u00e1x. filas (0 = todas)", value = 10000, min = 0, step = 1000)
          )
        ),
        shiny::actionButton(ns("btn_cargar_tabla"), "Cargar tabla",
          icon = shiny::icon("download"), class = "btn-success btn-sm"),
        shiny::div(class = "ds-hint mt-1",
                   shiny::textOutput(ns("msg_carga_tabla"))),
        shiny::hr(),
        shiny::actionButton(ns("btn_desconectar"), "Desconectar",
          icon = shiny::icon("plug-circle-xmark"),
          class = "btn-sm btn-outline-danger")
      )
    })

    # ── Cargar tabla ──────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_cargar_tabla, {
      shiny::req(con_activa(), input$tabla_sel, nchar(input$tabla_sel) > 0)

      nombre <- trimws(input$nombre_tabla_ds)
      if (nchar(nombre) == 0) nombre <- input$tabla_sel

      shiny::withProgress(message = paste0("Cargando '", input$tabla_sel, "'\u2026"), {
        result <- tryCatch({
          limite <- as.integer(input$limite_filas)
          df <- if (limite > 0) {
            DBI::dbGetQuery(
              con_activa(),
              paste0("SELECT * FROM ", input$tabla_sel, " FETCH FIRST ",
                     limite, " ROWS ONLY")
            )
          } else {
            DBI::dbReadTable(con_activa(), input$tabla_sel)
          }
          list(ok = TRUE, df = df)
        }, error = function(e) {
          # Fallback: intentar con LIMIT (MySQL/PostgreSQL/SQLite)
          tryCatch({
            limite <- as.integer(input$limite_filas)
            df <- if (limite > 0) {
              DBI::dbGetQuery(
                con_activa(),
                paste0("SELECT * FROM ", input$tabla_sel, " LIMIT ", limite)
              )
            } else {
              DBI::dbReadTable(con_activa(), input$tabla_sel)
            }
            list(ok = TRUE, df = df)
          }, error = function(e2) {
            list(ok = FALSE, msg = e2$message)
          })
        })

        if (result$ok) {
          df <- result$df
          datasets_rv$datasets[[nombre]] <- df
          assign(nombre, df, envir = .GlobalEnv)
          datasets_rv$seleccionado <- nombre
          active_dataset(df)
          active_name(nombre)
          output$msg_carga_tabla <- shiny::renderText(
            paste0("\u2713 Cargado: ", nrow(df), " filas x ", ncol(df), " columnas")
          )
          shiny::showNotification(
            paste0("'", nombre, "' cargado: ", nrow(df), " filas"),
            type = "message", duration = 3)
        } else {
          output$msg_carga_tabla <- shiny::renderText(
            paste0("\u2717 Error al cargar: ", result$msg)
          )
          shiny::showNotification(result$msg, type = "error", duration = 8)
        }
      })
    })

    # ── Desconectar ───────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_desconectar, {
      if (!is.null(con_activa())) {
        tryCatch(DBI::dbDisconnect(con_activa()), error = function(e) NULL)
      }
      con_activa(NULL)
      tablas_list(NULL)
      output$msg_conexion <- shiny::renderText("Desconectado.")
      shiny::showNotification("Conexi\u00f3n cerrada.", type = "message", duration = 2)
    })

    # ── Cerrar conexi\u00f3n al destruir el m\u00f3dulo (limpieza) ─────────────────
    shiny::onStop(function() {
      if (!is.null(con_activa())) {
        tryCatch(DBI::dbDisconnect(con_activa()), error = function(e) NULL)
      }
    })

  })
}
