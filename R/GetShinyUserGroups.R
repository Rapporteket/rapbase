#' Get user groups from a shiny session object
#' 
#' @inheritParams shinySessionInfo
#' 
#' @return String user groups
#' 
#' @seealso \code{\link{getShinyUserName}}, \code{\link{getShinyUserReshId}},
#' \code{\link{getShinyUserRole}}
#' 
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' getShinyUserGroups(shinySessionObject)
#' }
#' 
#' @export


getShinyUserGroups <- function(shinySession, testCase = FALSE) {
  shinySessionInfo(shinySession, entity = "groups", testCase)
}