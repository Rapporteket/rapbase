#' Display memory usage in the app
#'
#' This function creates a reactive text output for displaying memory usage.
#' In your server function, assign it to \code{output$memory}:
#'
#' \code{output$memory <- memoryDisplay(session)}
#'
#' The output ID \code{memory} is required and must match the textOutput
#' ID used in memoryTitle().
#'
#' @examples
#'     server <- function(input, output, session) {
#'       output$memory <- memoryDisplay(session)
#'     }
#'
#' @param session Shiny session object.
#'
#' @return A reactive text output displaying memory usage.
#' @export
memoryDisplay <- function(session = shiny::getDefaultReactiveDomain()) {
  shiny::renderText({
    shiny::invalidateLater(1000, session)
    mem <- ps::ps_memory_info(ps::ps_handle())["rss"] / 1024^2
    paste0("Memory: ", round(mem, 1), " MB")
  })
}

#' Title on the top left of the app, including the logo and memory usage
#'
#' @param title Title of the app
#'
#' @return a tagList containing the logo, the title, and memory usage
#'
#' @examples
#' app_ui <- function() {
#'   shiny::navbarPage(
#'     title = memoryTitle("My App")
#'   )
#' }
#' @export
memoryTitle <- function(title) {
  shiny::tagList(
    rapbase::regTitle(title),
    shiny::span(
      style = "margin-left:20px; font-size:12px;",
      shiny::textOutput("memory", inline = TRUE)
    )
  )
}
