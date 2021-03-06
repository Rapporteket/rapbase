#' Get user resh_id from a shiny session object
#'
#' @inheritParams shinySessionInfo
#'
#' @return String user resh_id
#'
#' @seealso \code{\link{getShinyUserName}}, \code{\link{getShinyUserGroups}},
#' \code{\link{getShinyUserRole}}
#'
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' getShinyUserReshId(shinySessionObject)
#' }
#'
#' @export


getShinyUserReshId <- function(shinySession, testCase = FALSE) {
  lifecycle::deprecate_warn(
    "1.10.0", "rapbase::GetShinyUserReshId()",
    "rapbase::getUserReshId()"
  )
  shinySessionInfo(shinySession, entity = "resh_id", testCase)
}
