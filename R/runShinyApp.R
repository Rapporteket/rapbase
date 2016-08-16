#' runShinyApp starts a shiny application in a package
#' 
#' runShinyApp start a shiny application residing within an R package. First
#' attempt is to keep this function generic (across packages). It might not
#' prove wise and future implementation might be per-package.
#' 
#' Idea and code (partly) taken from Dean Attali's
#' \href{http://www.r-bloggers.com/supplementing-your-r-package-with-a-shiny-app/}{R Blog}.
#' Gratitude.
#' 
#' @param appName String providing the name of the shiny application
#' @param appsDirectoryName String providing the within-package-path to its
#' shiny application(s). Assumingly, in the source tree for a given R package
#' an app will be found under \emph{inst/shinyApps/anApp}. In this case,
#' \emph{appsDirectoryName} must be set to \emph{'shinyApps'}.
#' @param packageName String providing the name of the package in which the
#' shiny app resides
#' @export

runShinyApp <- function(appName, appsDirectoryName, packageName) {
  
  # locate all shiny apps
  validApps <- list.files(system.file(appsDirectoryName,
                                      package = packageName))
  
  validAppsMsg <-
    paste0(
      "Valid apps are: '",
      paste(validApps, collapse = "', '"),
      "'")
  
  # if an invalid app is given, throw an error
  if (missing(appName) || !nzchar(appName) || !appName %in% validApps) {
    stop(
      'Please run `runShinyApp()` with a valid app name as an argument\n',
      validAppsMsg,
      call. = FALSE)
  }
  
  # find and launch the app
  appDir <- system.file(appsDirectoryName, appName, package = packageName)
  shiny::runApp(appDir, display.mode = "normal")
}