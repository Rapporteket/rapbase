#' Shiny modules providing GUI and server logic for Export
#'
#' Functions for registries that wants to implement exporting of registry
#' databases, \emph{e.g.} for local development purposes. Also includes
#' relevant helper functions
#'
#' @param id Character string module ID
#' @param registryName Character string registry name key
#' @param repoName Character string defining the github repository name of the
#' registry. Default value is \code{registryName}.
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
#'   exportUCServer("test", registryName = "rapbase")
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
exportUCServer <- function(id, registryName, repoName = registryName,
                           eligible = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    conf <- getConfig("rapbaseConfig.yml")

    pubkey <- shiny::reactive({
      shiny::req(input$exportPid)
      keys <- getGithub("keys", input$exportPid)
      sship::pubkey_filter(keys, "rsa")
    })

    encFile <- shiny::reactive({
      f <- exportDb(
        registryName,
        compress = input$exportCompress,
        session = session
      )
      message(paste("Dump file size:", file.size(f)))
      ef <- sship::enc(f,
        pid = NULL, pubkey_holder = NULL,
        pubkey = input$exportKey
      )
      ef
    })

    if (eligible) {
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

    ## UC
    output$exportPidUI <- shiny::renderUI({
      shiny::selectInput(
        shiny::NS(id, "exportPid"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("user")), "Velg mottaker:")
        ),
        choices = getGithub(
          "members",
          repoName,
          .token = conf$github$PAT$rapmaskin
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
      if (!eligible | length(pubkey()) == 0) {
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
exportUCApp <- function(registryName = "rapbase") {
  ui <- shiny::fluidPage(
    exportUCInput("rapbaseExport")
  )
  server <- function(input, output, session) {
    exportUCServer("rapbaseExport", registryName)
  }

  shiny::shinyApp(ui, server)
}


#' Shiny modules providing the Export Guide
#'
#' @param id Character string module ID
#' @param registryName Character string registry name key
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
exportGuideServer <- function(id, registryName) {
  shiny::moduleServer(id, function(input, output, session) {
    output$exportGuide <- shiny::renderUI({
      renderRmd(
        sourceFile = system.file("exportGuide.Rmd", package = "rapbase"),
        outputType = "html_fragment",
        params = list(registryName = registryName)
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
exportDb <- function(registryName, compress = FALSE, session) {
  stopifnot(Sys.which("mysqldump") != "")
  stopifnot(Sys.which("gzip") != "")

  f <- tempfile(pattern = registryName, fileext = ".sql")
  conf <- rapbase::getConfig()[[registryName]]
  cmd <- paste0(
    "mysqldump ",
    "--no-tablespaces --single-transaction --add-drop-database "
  )
  cmd <- paste0(
    cmd, "-B -u ", conf$user, " -p", conf$pass, " -h ", conf$host,
    " ", conf$name, " > ", f
  )
  invisible(system(cmd))

  if (compress) {
    inFile <- f
    f <- paste0(inFile, ".gz")
    cmd <- paste("gzip -f", inFile, ">", f)
    invisible(system(cmd))
  }

  repLogger(session, msg = paste(registryName, "Db dump created."))

  invisible(f)
}
