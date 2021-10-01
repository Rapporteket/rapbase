#' Shiny modules providing the Stats Guide
#'
#' @param id Character string module ID
#' @param registryName Character string registry name key
#'
#' @return Functions ui and server representing the (module) app
#' @name statsGuide
#' @aliases statsGuideUI statsGuideServer statsGuideApp
#' @examples
#' ui <- shiny::fluidPage(
#'   statsGuideUI("statsGuide")
#' )
#'
#' server <- function(input, output, session) {
#'   statsGuideServer("statsGuide", "test")
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
NULL

#' @rdname statsGuide
#' @export
statsGuideUI <- function(id) {

  shiny::htmlOutput(shiny::NS(id, "statsGuide"))
}

#' @rdname exportGuide
#' @export
statsGuideServer <- function(id, registryName) {

  shiny::moduleServer(id, function(input, output, session) {

    output$statsGuide <- shiny::renderUI({
      rapbase::renderRmd(
        sourceFile = system.file("statsGuide.Rmd", package = "rapbase"),
        outputType = "html_fragment",
        params = list(registryName = registryName))
    })
  })
}

#' @rdname exportGuide
#' @export
statsGuideApp <- function() {

  ui <- shiny::fluidPage(
    statsGuideUI("statsGuide")
  )

  server <- function(input, output, session) {
    statsGuideServer("statsGuide", "test")
  }

  shiny::shinyApp(ui, server)
}
