#' Shiny modules providing GUI and server logic for Export
#'
#' Functions for registries that wants to implement exporting of registry
#' databases, \emph{e.g.} for local development purposes. Also includes
#' relevant helper functions
#'
#' @param id Character string module ID
#' @param registryName Character string registry name key
#' @param pubkey Character vector with public keys
#' @param compress Logical if export data is to be compressed (using gzip).
#' FALSE by default.
#' @param session Shiny session object
#'
#' @return Shiny objects, mostly. Helper functions may return other stuff too.
#' @name export
#' @aliases exportUCInput exportUCServer exportUCApp selectListPubkey exportDb
NULL

# shiny modules
#' @rdname export
#' @export
exportUCInput <- function(id) {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "exportPidUI")),
    shiny::uiOutput(shiny::NS(id, "exportKeyUI")),
    shiny::checkboxInput(shiny::NS(id, "exportCompress"), "Komprimer eksport"),
    shiny::uiOutput(shiny::NS(id, "exportEncryptUI")),
    shiny::uiOutput(shiny::NS(id, "exportDownloadUI"))
  )
}

#' @rdname export
#' @export
exportUCServer <- function(id, registryName) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      exportFile = NULL,
      exportText = "Kryptér",
      exportIcon = "lock-open",
      exportClass = "btn-warning"
    )

    pubkey <- shiny::reactive({
      shiny::req(input$exportPid)
      rapbase::getGithub("keys", input$exportPid)
    })

    ## observers
    shiny::observeEvent(input$exportPid, {
      rv$exportFile <- NULL
      rv$exportText <- "Kryptér"
      rv$exportIcon <- "lock-open"
      rv$exportClass <- "btn-warning"
    })
    shiny::observeEvent(input$exportKey, {
      rv$exportFile <- NULL
      rv$exportText <- "Kryptér"
      rv$exportIcon <- "lock-open"
      rv$exportClass <- "btn-warning"
    })
    shiny::observeEvent(input$exportCompress, {
      rv$exportFile <- NULL
      rv$exportText <- "Kryptér"
      rv$exportIcon <- "lock-open"
      rv$exportClass <- "btn-warning"
    })

    shiny::observeEvent(input$exportEncrypt, {
      if (is.null(rv$exportFile)) {
        f <- rapbase::exportDb(registryName,
                               compress = input$exportCompress,
                               session = session)
        rv$exportFile <- sship::enc(f, pid = NULL, pubkey = input$exportKey)
        rv$exportText <- "Kryptert!"
        rv$exportIcon <- "lock"
        rv$exportClass <- "btn-success"
      }
    })

    output$exportDownload <- shiny::downloadHandler(
      filename = basename(rv$exportFile),
      content = function(file) {
        file.copy(rv$exportFile, file)
        rapbase::repLogger(
          session,
          msg = paste("Db export file", basename(rv$exportFile), "downloaded"))
      }
    )

    ## UC
    output$exportPidUI <- shiny::renderUI({
      shiny::selectInput(
        shiny::NS(id, "exportPid"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("user")), "Velg mottaker:")
        ),
        choices =  rapbase::getGithub("contributors", registryName))
    })
    output$exportKeyUI <- shiny::renderUI({
      if (length(pubkey()) == 0) {
        shiny::p("No keys found!")
      } else {
        shiny::selectInput(
          shiny::NS(id, "exportKey"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("key")), "Velg nøkkel:")
          ),
          choices = rapbase::selectListPubkey(pubkey()))
      }
    })
    output$exportEncryptUI <- shiny::renderUI({
      shiny::actionButton(
        shiny::NS(id, "exportEncrypt"),
        label = rv$exportText,
        icon = shiny::icon(rv$exportIcon),
        class = rv$exportClass)
    })
    output$exportDownloadUI <- shiny::renderUI({
      if (is.null(rv$exportFile)) {
        NULL
      } else {
        shiny::tagList(
          shiny::hr(),
          shiny::downloadButton(
            shiny::NS(id, "exportDownload"),
            label = "Last ned!")
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


# the rest is helper functions

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
  cmd <- "mysqldump --no-tablespaces --single-transaction --add-drop-database "
  cmd <- paste0(cmd, "-B -u ", conf$user, " -p", conf$pass, " -h ", conf$host,
         " ", conf$name, " > ", f)
  invisible(system(cmd))

  if (compress) {
    inFile <- f
    f <- paste0(inFile, ".gz")
    cmd <- paste("gzip -f", inFile, ">", f)
    invisible(system(cmd))
  }

  rapbase::repLogger(session, msg = paste(registryName, "db dump created."))

  invisible(f)
}


