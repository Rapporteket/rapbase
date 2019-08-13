#' Get user role from a shiny session object
#' 
#' @inheritParams shinySessionInfo
#' 
#' @return String user role
#' 
#' @seealso \code{\link{getShinyUserName}}, \code{\link{getShinyUserGroups}},
#' \code{\link{getShinyUserReshId}}
#' 
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' getShinyUserRole(shinySessionObject)
#' }
#' 
#' @export


getShinyUserRole <- function(shinySession, testCase = FALSE) {
  shinySessionInfo(shinySession, entity = "role", testCase)
}