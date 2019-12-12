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
  lifecycle::deprecate_warn("1.10.0", "rapbase::GetShinyUserName()",
                 "rapbase::getUserName()")
  shinySessionInfo(shinySession, entity = "user", testCase)
}