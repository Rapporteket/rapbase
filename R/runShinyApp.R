#' runShinyApp starts a shiny application in a package
#' 
#' runShinyApp start a shiny application residing within an R package. First
#' attempt is to keep thin function generic (across packages). It might not
#' prove wise and future implementation might be per-package.
#' 
#' @param appName String providing the name of the shiny application
#' @param appsDirectoryName String providing the within-package-path to its
#' shiny application(s). Assumingly, in the source tree for a given R package
#' an app will be found under \emph{inst/shinyApps/anApp}. In this case,
#' \emph{appsDirectoryName} must be set to \emph{'shinyApps'}.
#' @param packageName String providing the name of the package in which the
#' shiny app resides

runShinyApp <- function(appName, appsDirectoryName, packageName) {
  
  
}