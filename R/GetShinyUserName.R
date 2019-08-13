#' Get user name from a shiny session object
#' 
#' @inheritParams shinySessionInfo
#' 
#' @return String user name
#' 
#' @seealso \code{\link{getShinyUserName}}, \code{\link{getShinyUserGroups}},
#' \code{\link{getShinyUserReshId}}, \code{\link{getShinyUserRole}}
#' 
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' getShinyUserName(shinySessionObject)
#' }
#' 
#' @export


getShinyUserName <- function(shinySession, testCase = FALSE) {
  shinySessionInfo(shinySession, entity = "user", testCase)
}