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
  lifecycle::deprecate_warn(
    "1.10.0", "rapbase::GetShinyUserGroups()",
    "rapbase::getUserGroups()"
  )
  shinySessionInfo(shinySession, entity = "groups", testCase)
}
