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
#' @param version Version of bootstrap to use (3, 4 or 5).
#'
#' @return a Bootstrap theme object
#' @export
#'
theme <- function(theme = "flatly", version = 3) {
  bslib::bs_theme(bootswatch = theme, version = version)
}
