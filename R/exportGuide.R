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
      rapbase::renderRmd(
        sourceFile = system.file("exportGuide.Rmd", package = "rapbase"),
        outputType = "html_fragment",
        params = list(registryName = registryName))
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
