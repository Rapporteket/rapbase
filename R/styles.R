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


#' DO NOT USE THIS FUNCTION, use regTitle instead.
#' This is only for backward compatibility.
#'
#' @param ... regTitle arguments
#'
#' @return a div containing the logo and the title
#' @export
#'
title <- function(...) {
  regTitle(...)
}

#' DO NOT USE THIS FUNCTION, use rapTheme instead.
#' This is only for backward compatibility.
#'
#' @param ... rapTheme arguments
#'
#' @return a Bootstrap theme object
#' @export
#'
theme <- function(...) {
  rapTheme(...)
}
