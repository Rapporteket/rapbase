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
#' @param session Shiny session object
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
    shiny::checkboxInput(shiny::NS(id, "exportCompress"), "Komprimer eksport"),
    shiny::uiOutput(shiny::NS(id, "exportDownloadUI"))
  )
}

#' @rdname export
#' @export
exportUCServer <- function(id, dbName, teamName = dbName) {
  shiny::moduleServer(id, function(input, output, session) {

    pubkey <- shiny::reactive({
      shiny::req(input$exportPid)
      keys <- getGithub("keys", input$exportPid)
      sship::pubkey_filter(keys, "rsa")
    })

    encFile <- shiny::reactive({
      f <- exportDb(
        dbName,
        compress = input$exportCompress,
        session = session
      )
      message(paste("Dump file size:", file.size(f)))
      ef <- sship::enc(
        f,
        pid = NULL,
        pubkey_holder = NULL,
        pubkey = input$exportKey
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
    output$exportPidUI <- shiny::renderUI({
      shiny::selectInput(
        shiny::NS(id, "exportPid"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("user")), "Velg mottaker:")
        ),
        choices = getGithub(
          "members",
          teamName,
          .token = Sys.getenv("GITHUB_PAT")
        )
      )
    })
    output$exportKeyUI <- shiny::renderUI({
      if (length(pubkey()) == 0) {
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
    output$exportDownloadUI <- shiny::renderUI({
      if (length(pubkey()) == 0) {
        shiny::tagList(
          shiny::hr(),
          shiny::h4("Funksjon utilgjengelig"),
          shiny::p("Kontakt registeret")
        )
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
  })
}

#' @rdname export
#' @export
exportUCServer2 <- function(id, dbName, teamName,
                            eligible = shiny::reactiveVal(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {

    pubkey <- shiny::reactive({
      shiny::req(input$exportPid)
      keys <- getGithub("keys", input$exportPid)
      sship::pubkey_filter(keys, "rsa")
    })


    encFile <- shiny::reactive({
      f <- exportDb(
        dbName(),
        compress = input$exportCompress,
        session = session
      )
      message(paste("Dump file size:", file.size(f)))
      ef <- sship::enc(
        f,
        pid = NULL,
        pubkey_holder = NULL,
        pubkey = input$exportKey
      )
      ef
    })

    shiny::observeEvent(eligible(), {
      if (eligible()) {
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
      }
    }
    )

    ## UC
    output$exportPidUI <- shiny::renderUI({
      shiny::selectInput(
        shiny::NS(id, "exportPid"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("user")), "Velg mottaker:")
        ),
        choices = getGithub(
          "members",
          teamName,
          .token = Sys.getenv("GITHUB_PAT")
        )
      )
    })
    output$exportKeyUI <- shiny::renderUI({
      if (length(pubkey()) == 0) {
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
    output$exportDownloadUI <- shiny::renderUI({
      if (!eligible() | length(pubkey()) == 0) {
        shiny::tagList(
          shiny::hr(),
          shiny::h4("Funksjon utilgjengelig"),
          shiny::p("Kontakt registeret")
        )
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
  listName <- substr(pubkey, nchar(pubkey) - 7, nchar(pubkey))
  listName <- paste0(substr(pubkey, 1, 8), "...", listName)
  names(pubkey) <- listName

  as.list(pubkey)
}

#' @rdname export
#' @export
exportDb <- function(dbName, compress = FALSE, session) {
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

  repLogger(session, msg = paste(conf$name, "Db dump created."))

  invisible(f)
}
