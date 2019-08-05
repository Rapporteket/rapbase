#' Get user full name from config or session object
#' 
#' This is a helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to this function is made without any arguments. If
#' redefining contexts is needed, please use \code{\link{userInfo}} instead.
#' 
#' @inheritParams userInfo
#' 
#' @return String full name
#' 
#' @seealso \code{\link{getUserName}},
#' \code{\link{getUserGroups}}, \code{\link{getUserReshId}},
#' \code{\link{getUserEmail}}, \code{\link{getUserPhone}}
#' 
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' getUserFullName()
#' getUserFullName(shinySessionObject)
#' }
#' 
#' @export


getUserFullName <- function(shinySession = NULL) {
  
  if (missing(shinySession)) {
    warning(paste("A shinySession object was not provided. Hence, this",
                  "function call may not work across all contexts"))
  }
  
  userInfo(shinySession, entity = "full_name")
  
}