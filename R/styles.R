#' Title on the top left of the app, including the logo
#'
#' @param regTitle Title of the app
#'
#' @return a div containing the logo and the title
#' @export
#'
regTitle <- function(regTitle = "rapbase") {
  shiny::div(
    style = "display: inline-block;",
    shiny::a(
      style = paste(
        "vertical-align: middle;",
        "float: left;",
        "width: 26px;",
        "height: 26px;",
        " fill: #18bc9c;"
      ),
      shiny::includeHTML(
        system.file("www/logo.svg", package = "rapbase")
      )
    ),
    regTitle
  )
}

#' Theme of the app.
#'
#' This is a wrapper around bslib::bs_theme
#'
#' @param theme Name of the theme. See bslib::bootswatch_themes()
#' for available themes.
#' @param version Version of bootstrap to use (3, 4 or 5).
#'
#' @return a Bootstrap theme object
#' @export
#'
rapTheme <- function(theme = "flatly", version = 3) {
  bslib::bs_theme(bootswatch = theme, version = version)
}

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
