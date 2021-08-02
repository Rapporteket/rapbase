#' Shiny modules and helper functions for registry usage reports
#'
#' These modules may be used by registries for easy setup of usage reports.
#' The intended purpose is to provide registry staff access to when and by
#' whom the resources at Rapporteket were used, \emph{i.e.} application
#' start-up and single report usage. As such, this will be a tool to provide
#' useful statistics. However, it might also serve as a formal logging utility
#' but only if carefully implemented throughout the relevant functions that
#' make up the registry application at Rapporteket.
#'
#' @param id Character string shiny module id
#' @param registryName Character string registry name key
#'
#' @return Shiny objects, mostly. Helper functions may return other stuff too.
#' @name stats
#' @aliases statsInput statsUI statsServer statsApp
#'
#' @examples
#' # client user interface function
#' ui <- shiny::fluidPage(
#'   shiny::sidebarLayout(
#'     shiny::sidebarPanel(statsInput("test")),
#'     shiny::mainPanel(statsUI("test"))
#'   )
#' )
#'
#' # server function
#' server <- function(input, output, session) {
#'   statsServer("test", registryName = "rapbase")
#' }
#'
#' # run the shiny app in an interactive environment
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
NULL

#' @rdname stats
#' @export
statsInput <- function(id) {

  shiny::tagList(
    shiny::radioButtons(
      shiny::NS(id, "stats"), "Kategori:",
      choices = c("Innlogging", "Enkeltrapporter")
    ),
    shiny::uiOutput(shiny::NS(id, "period")),
    shiny::radioButtons(
      shiny::NS(id, "downloadFormat"), "Nedlastingsformat:",
      choices = list(csv = "csv", csvNordic = "csv-nordisk")
    ),
    shiny::uiOutput(shiny::NS(id, "download"))
  )
}

#' @rdname stats
#' @export
statsUI <- function(id) {

  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "pivot"))
  )
}


#' @rdname stats
#' @export
statsServer <- function(id, registryName) {

  shiny::moduleServer(id, function(input, output, session) {

    output$period <- shiny::renderUI({
      shiny::dateRangeInput(
        shiny::NS(id, "period"), "Periode:",
        start = Sys.Date(), end = Sys.Date(), separator = "-"
      )
    })
    output$download <- shiny::renderUI({
      shiny::downloadButton(
        shiny::NS(id, "download"), "Last ned!"
      )
    })
    output$pivot <- shiny::renderUI({
      NULL
    })
  })
}

#' @rdname stats
#' @export
statsApp <- function() {

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(statsInput("test")),
      shiny::mainPanel(statsUI("test"))
    )
  )

  server <- function(input, output, session) {
    statsServer("test", registryName = "rapbase")
  }

  shiny::shinyApp(ui, server)
}
