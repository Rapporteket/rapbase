#' Shiny modules providing GUI and server logic for Export
#'
#' Functions for registries that wants to implement exporting of registry
#' databases, \emph{e.g.} for local development purposes. Also includes
#' relevant helper functions
#'
#' @param id Character string module ID
#' @param registryName Character string registry name key
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
exportUCServer <- function(id, registryName, eligible = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {

    pubkey <- shiny::reactive({
      shiny::req(input$exportPid)
      rapbase::getGithub("keys", input$exportPid)
    })

    encFile <- shiny::reactive({
      f <- rapbase::exportDb(registryName,
                             compress = input$exportCompress,
                             session = session)
      ef <- sship::enc(f, pid = NULL, pubkey_holder = NULL,
                       pubkey = input$exportKey)
      ef
    })

    if (eligible) {
      output$exportDownload <- shiny::downloadHandler(
        filename = function() {
          basename(encFile())
        },
        content = function(file) {
          file.copy(encFile(), file)
          rapbase::repLogger(
            session,
            msg = paste("Db export file", basename(encFile()), "downloaded."))
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
        choices =  rapbase::getGithub("contributors", registryName))
    })
    output$exportKeyUI <- shiny::renderUI({
      if (length(pubkey()) == 0) {
        shiny::p("No keys found!")
      } else {
        shiny::selectInput(
          shiny::NS(id, "exportKey"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("key")), "Velg \u00f8kkel:")
          ),
          choices = rapbase::selectListPubkey(pubkey()))
      }
    })
    output$exportDownloadUI <- shiny::renderUI({
      if (!eligible) {
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
  cmd <- paste0("MYSQL_PWD=", conf$pass, " mysqldump --column-statistics=0 ",
               "--no-tablespaces --single-transaction --add-drop-database ")
  cmd <- paste0(cmd, "-B -u ", conf$user, " -h ", conf$host, " ", conf$name,
                " > ", f)
  invisible(system(cmd))

  if (compress) {
    inFile <- f
    f <- paste0(inFile, ".gz")
    cmd <- paste("gzip -f", inFile, ">", f)
    invisible(system(cmd))
  }

  rapbase::repLogger(session, msg = paste(registryName, "Db dump created."))

  invisible(f)
}
