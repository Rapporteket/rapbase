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
        style = "vertical-align: middle; float: left; width: 26px; height: 26px; fill: #18bc9c;",
        shiny::includeHTML(
      system.file("www/logo.svg", package = "rapbase")
    )),
    regTitle
  )
}

#' Theme of the app. Default is "flatly"
#'
#' @param theme
#'
#' @return a shinytheme object
#' @export
#'
theme <- function(theme = "flatly") {
  shinythemes::shinytheme(theme)
}
