#' Get user role from a shiny session object
#'
#' @inheritParams shinySessionInfo
#'
#' @return String user role
#'
#' @seealso \code{\link{getShinyUserName}}, \code{\link{getShinyUserGroups}},
#' \code{\link{getShinyUserReshId}}
#'
#' @export


getShinyUserRole <- function(shinySession, testCase = FALSE) {
  lifecycle::deprecate_warn(
    "1.10.0", "rapbase::GetShinyUserRole()",
    "rapbase::getUserRole()"
  )
  shinySessionInfo(shinySession, entity = "role", testCase)
}
