#' Shiny modules providing GUI and server logic for Export
#'
#' Functions for registries that wants to implement exporting of registry
#' databases, \emph{e.g.} for local development purposes. Also includes
#' relevant helper functions
#'
#' @param id Character string module ID
#' @param dbName Character string database name. If this is `data`, then the
#' database name is taken from the environment variable `MYSQL_DB_DATA`.
#' @param teamName Character string defining the github team name containing
#' members allowed to export the database. Default value is \code{dbName}.
#' @param eligible Logical defining if the module should be allowed to work at
#' full capacity. This might be useful when access to module products should be
#' restricted. Default is TRUE, \emph{i.e.} no restrictions.
#' @param pubkey Character vector with public keys
#' @param compress Logical if export data is to be compressed (using gzip).
#' FALSE by default.
#' @param user navbarWidgetServer2 user object, used for logging. Default is NULL
#' in which case no logging will be done.
#'
#' @return Shiny objects, mostly. Helper functions may return other stuff too.
#' @name export
#' @aliases exportUCInput exportUCServer exportUCApp selectListPubkey exportDb
#' @examples
#' ## client user interface function
#' ui <- shiny::fluidPage(
#'   shiny::sidebarLayout(
#'     shiny::sidebarPanel(
#'       exportUCInput("test"),
#'     ),
#'     shiny::mainPanel(
#'       NULL
#'     )
#'   )
#' )
#'
#' ## server function
#' server <- function(input, output, session) {
#'   exportUCServer("test", dbName = "rapbase")
#' }
#'
#' ## run the shiny app in an interactive environment
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
NULL

# shiny modules
#' @rdname export
#' @export
exportUCInput <- function(id) {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "exportPidUI")),
    shiny::uiOutput(shiny::NS(id, "exportKeyUI")),
    shiny::uiOutput(shiny::NS(id, "exportFullDbOrTable")),
    shiny::uiOutput(shiny::NS(id, "exportTable")),
    shiny::uiOutput(shiny::NS(id, "exportCompressUI")),
    shiny::uiOutput(shiny::NS(id, "exportDownloadUI"))
  )
}

#' @rdname export
#' @export
exportUCServer <- function(
  id, dbName, teamName = NULL,
  eligible = shiny::reactiveVal(TRUE),
  user = NULL
) {
  ns <- shiny::NS(id)

  if (!shiny::is.reactive(eligible)) {
    # make eligible reactive if not already
    eligible <- shiny::reactiveVal(eligible)
  }

  if (!shiny::is.reactive(dbName)) {
    if (is.null(teamName)) {
      teamName <- dbName
    }
    # make dbName reactive if not already
    dbName <- shiny::reactiveVal(dbName)
  }

  shiny::moduleServer(id, function(input, output, session) {

    pubkey <- shiny::reactive({
      shiny::req(input$exportPid)
      keys <- getGithub("keys", input$exportPid)
      sship::pubkey_filter(keys, "rsa")
    })

    encFile <- shiny::reactive({
      shiny::req(dbName(), input$exportKey)
      if (input$fullDb == "Hele databasen") {
        f <- exportDb(
          dbName(),
          compress = input$exportCompress,
          user = user
        )
      } else {
        shiny::req(input$dataType)
        f <- queryToFile(
          dbName(),
          downloadDataQuery(),
          format = input$dataType,
          compress = input$exportCompress,
          user = user
        )
      }

      message(paste("Plain file size:", file.size(f)))
      ef <- sship::enc(
        filename      = f,
        pid           = NULL,
        pubkey_holder = NULL,
        pubkey        = input$exportKey
      )

      ef
    })

    output$exportDownload <- shiny::downloadHandler(
      filename = function() {
        basename(encFile())
      },
      content = function(file) {
        file.copy(encFile(), file)
        repLogger(
          session,
          msg = paste("Db export file", basename(encFile()), "downloaded.")
        )
      }
    )


    ## UC
    output$exportFullDbOrTable <- shiny::renderUI({
      shiny::req(pubkey, eligible)
      if (length(pubkey()) == 0 | !eligible()) {
        NULL
      } else {
        shiny::radioButtons(
          shiny::NS(id, "fullDb"),
          "",
          c("Hele databasen", "Enkelttabell"),
          selected = shiny::isolate(input$fullDb) %||% "Hele databasen",
          inline = TRUE
        )
      }
    })
    output$exportPidUI <- shiny::renderUI({
      shiny::req(eligible)
      teamMembers <- tryCatch(
        getGithub(
          "members",
          teamName,
          .token = Sys.getenv("GITHUB_PAT")
        ),
        error = function(e) {
          message("Error fetching team members from GitHub: ", e$message)
          c()
        }
      )
      if (!eligible()) {
        shiny::tagList(
          shiny::h4("Funksjon utilgjengelig"),
          shiny::p("Kontakt registeret")
        )
      } else if (length(teamMembers) == 0) {
        shiny::p(
          "Ingen team-medlemmer funnet. Sjekk om team ",
          shiny::strong(teamName), " finnes og inneholder medlemmer."
        )
      } else {
        shiny::selectInput(
          shiny::NS(id, "exportPid"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("user")), "Velg mottaker:")
          ),
          choices = teamMembers
        )
      }
    })
    output$exportKeyUI <- shiny::renderUI({
      shiny::req(pubkey, eligible)
      if (!eligible()) {
        NULL
      } else if (length(pubkey()) == 0) {
        shiny::p("No keys found!")
      } else {
        shiny::selectInput(
          shiny::NS(id, "exportKey"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("key")), "Velg n\u00f8kkel:")
          ),
          choices = selectListPubkey(pubkey())
        )
      }
    })
    output$exportCompressUI <- shiny::renderUI({
      shiny::req(pubkey, eligible)
      if (length(pubkey()) == 0 | !eligible()) {
        NULL
      } else {
        shiny::checkboxInput(
          shiny::NS(id, "exportCompress"),
          "Komprimer eksport"
        )
      }
    })
    output$exportDownloadUI <- shiny::renderUI({
      shiny::req(pubkey, eligible)
      if (length(pubkey()) == 0 | !eligible()) {
        NULL
      } else {
        shiny::tagList(
          shiny::hr(),
          shiny::downloadButton(
            shiny::NS(id, "exportDownload"),
            label = "Last ned!"
          )
        )
      }
    })

    shiny::observeEvent(input$fullDb, {
      if (input$fullDb == "Hele databasen") {
        output$exportTable <- NULL
      } else {
        output$exportTable <- shiny::renderUI({
          shiny::tagList(
            shiny::uiOutput(ns("dataTabNames")),
            shiny::uiOutput(ns("dateTimeCols")),
            shiny::uiOutput(ns("dateFilterUI")),
            shiny::uiOutput(ns("dataType"))
          )
        })
      }
    })

    meta <- shiny::reactive({
      describeRegistryDb(registryName = dbName())
    })

    output$dataTabNames <- shiny::renderUI({
      tabs <- names(meta())
      shiny::selectInput(
        inputId = ns("dataTab"),
        label = "Velg tabell:",
        choices = tabs,
        selected = tabs[1]
      )
    })

    output$dataType <- shiny::renderUI({
      shiny::selectInput(
        inputId = ns("dataType"),
        label = "Velg datatype:",
        choices = c("RDS", "CSV")
      )
    })

    output$dateTimeCols <- shiny::renderUI({
      query <- paste0("SELECT COLUMN_NAME
        FROM INFORMATION_SCHEMA.COLUMNS
        WHERE DATA_TYPE IN (
            'date',
            'datetime',
            'datetime2',
            'smalldatetime',
            'datetimeoffset'
        )
        AND TABLE_NAME = '", input$dataTab, "';")
      dateTimeCols <- loadRegData(registryName = dbName(), query)
      dateTimeCols <- c("Ingen datofilter", dateTimeCols$COLUMN_NAME)
      shiny::selectInput(ns("dateColSelect"), "Velg datofilter:", dateTimeCols)
    })

    output$dateFilterUI <- shiny::renderUI({
      shiny::req(input$dateColSelect)
      if (input$dateColSelect != "Ingen datofilter") {
        shiny::dateRangeInput(
          ns("dateRange"),
          "Velg datofilter:",
          start = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
          end = Sys.Date()
        )
      }
    })

    downloadDataQuery <- shiny::reactive({
      shiny::req(input$dataTab)
      base_sql <- paste0("SELECT * FROM ", input$dataTab)

      # No filter selected
      if (is.null(input$dateColSelect) ||
            input$dateColSelect == "Ingen datofilter") {
        return(paste0(base_sql, ";"))
      }

      shiny::req(input$dateRange)
      shiny::req(!any(is.na(input$dateRange)))

      start <- input$dateRange[1]
      end   <- input$dateRange[2]

      paste0(
        base_sql,
        " WHERE ",
        input$dateColSelect,
        " >= '", start,
        "' AND ",
        input$dateColSelect,
        " < DATE_ADD('", end, "', INTERVAL 1 DAY);"
      )

    })

  })
}

#' @rdname export
#' @export
exportUCApp <- function(dbName = "rapbase") {
  ui <- shiny::fluidPage(
    exportUCInput("rapbaseExport")
  )
  server <- function(input, output, session) {
    exportUCServer("rapbaseExport", dbName)
  }

  shiny::shinyApp(ui, server)
}


#' Shiny modules providing the Export Guide
#'
#' @param id Character string module ID
#' @param dbName Character string registry name key
#'
#' @return Functions ui and server representing the (module) app
#' @name exportGuide
#' @aliases exportGuideUI exportGuideServer exportGuideApp
#' @examples
#' ui <- shiny::fluidPage(
#'   exportGuideUI("exportGuide")
#' )
#'
#' server <- function(input, output, session) {
#'   exportGuideServer("exportGuide", "test")
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
NULL

#' @rdname exportGuide
#' @export
exportGuideUI <- function(id) {
  shiny::htmlOutput(shiny::NS(id, "exportGuide"))
}

#' @rdname exportGuide
#' @export
exportGuideServer <- function(id, dbName) {
  shiny::moduleServer(id, function(input, output, session) {
    output$exportGuide <- shiny::renderUI({
      renderRmd(
        sourceFile = system.file("exportGuide.Rmd", package = "rapbase"),
        outputType = "html_fragment",
        # sqlite is TRUE to avoid check about missing env vars
        params = list(dbName = getDbConfig(dbName, sqlite = TRUE)$name)
      )
    })
  })
}

#' @rdname exportGuide
#' @export
exportGuideServer2 <- function(id, dbName) {
  shiny::moduleServer(id, function(input, output, session) {
    output$exportGuide <- shiny::renderUI({
      renderRmd(
        sourceFile = system.file("exportGuide.Rmd", package = "rapbase"),
        outputType = "html_fragment",
        params = list(dbName = dbName())
      )
    })
  })
}

#' @rdname exportGuide
#' @export
exportGuideApp <- function() {
  ui <- shiny::fluidPage(
    exportGuideUI("exportGuide")
  )

  server <- function(input, output, session) {
    exportGuideServer("exportGuide", "test")
  }

  shiny::shinyApp(ui, server)
}


# helper functions

#' @rdname export
#' @export
selectListPubkey <- function(pubkey) {
  if (!is.character(pubkey)) return(list())
  listName <- substr(pubkey, nchar(pubkey) - 7, nchar(pubkey))
  listName <- paste0(substr(pubkey, 1, 8), "...", listName)
  names(pubkey) <- listName

  as.list(pubkey)
}

#' @rdname export
#' @export
exportDb <- function(dbName, compress = FALSE, user) {
  stopifnot(Sys.which("mysqldump") != "")
  stopifnot(Sys.which("gzip") != "")

  conf <- getDbConfig(dbName)
  f <- tempfile(pattern = conf$name, fileext = ".sql")

  cmd <- paste0(
    "mysqldump ",
    "--no-tablespaces --single-transaction --add-drop-database "
  )
  cmd <- sprintf(
    "%s -B -u %s -p'%s' -h %s %s > %s",
    cmd, conf$user, conf$pass, conf$host, conf$name, f
  )
  invisible(system(cmd))

  if (compress) {
    inFile <- f
    f <- paste0(inFile, ".gz")
    cmd <- paste("gzip -f", inFile, ">", f)
    invisible(system(cmd))
  }

  repLogger2(user, msg = paste(conf$name, "Db dump created."))

  invisible(f)
}

queryToFile <- function(dbName,
                        query,
                        format = c("RDS", "CSV"),
                        compress = FALSE,
                        user) {
  format <- match.arg(format)

  conf <- getDbConfig(dbName)

  ext <- switch(
    format,
    RDS = if (compress) ".rds.gz" else ".rds",
    CSV = if (compress) ".csv.gz" else ".csv"
  )

  out <- tempfile(fileext = ext)
  dat <- loadRegData(registryName = dbName, query = query)

  if (format == "RDS") {
    if (compress) {
      gz <- gzfile(out, open = "wb")
      on.exit(close(gz), add = TRUE)
      saveRDS(dat, gz)
    } else {
      saveRDS(dat, out)
    }
  }

  if (format == "CSV") {
    if (compress) {
      gz <- gzfile(out, open = "wt")
      on.exit(close(gz), add = TRUE)
      utils::write.csv2(dat, gz, row.names = FALSE, na = "")
    } else {
      utils::write.csv2(dat, out, row.names = FALSE, na = "")
    }
  }

  if (!is.null(user)) {
    repLogger2(
      user = user,
      msg = paste(conf$name, "Query", format, "created.")
    )
  }

  out
}
