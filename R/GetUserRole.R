#' Get user role from config or session object
#' 
#' This is a helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to this function is made without any arguments. If
#' redefining contexts is needed, please use \code{\link{userInfo}} instead.
#' 
#' @inheritParams userInfo
#' 
#' @return String user name
#' 
#' @seealso \code{\link{getUserName}},
#' \code{\link{getUserGroups}}, \code{\link{getUserReshId}}
#' 
#' @examples
#' \dontrun{
#' getUserRole()
#' getUserRole(shinySessionObject)
#' }
#' 
#' @export


getUserRole <- function(shinySession = NULL) {
  userInfo(shinySession, entity = "role")
}