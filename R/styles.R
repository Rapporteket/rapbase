#' Title on the top left of the app, including the logo
#'
#' @param regTitle Title of the app
#'
#' @return a div containing the logo and the title
#' @export
#'
title <- function(regTitle = "rapbase") {
  shiny::div(
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
#'
#' @return a shinytheme object
#' @export
#'
theme <- function(theme = "flatly", version = 3) {
  bslib::bs_theme(bootswatch = theme, version = version)
}
