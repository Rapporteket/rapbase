#' Get user groups from config or session object
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
#' \code{\link{getUserReshId}}, \code{\link{getUserRole}}
#' 
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' getUserGroups()
#' getUserGroups(shinySessionObject)
#' }
#' 
#' @export


getUserGroups <- function(shinySession = NULL) {
  
  if (missing(shinySession)) {
    warning(paste("A shinySession object was not provided. Hence, this",
                  "function call may not work across all contexts"))
  }
  
  userInfo(shinySession, entity = "groups")
  
}