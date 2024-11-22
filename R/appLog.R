#' Settings for logging as json
#'
#' Every info, warning and error will be logged in json format.
#'
#' @param usernameEnv Global variable containing user name
#' @param appidEnv Global variable containing application name
#' @param testing Is the if function running in a test?
#' Function will skip some calls if it does.
#'
#' @export
#'
loggerSetup <- function(
  usernameEnv = "SHINYPROXY_USERNAME",
  appidEnv = "SHINYPROXY_APPID",
  testing = FALSE
) {
  logger::log_threshold(logger::INFO)
  formatterJson <- function(level, message, ...) {
    username <- Sys.getenv(usernameEnv, unset = "unknown")
    appid <- Sys.getenv(appidEnv, unset = "unknown")
    return(jsonlite::toJSON(
      list(
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
        level = attr(level, "level"),
        app = appid,
        user = username,
        message = message
      ),
      auto_unbox = TRUE
    )
    )
  }
  logger::log_layout(formatterJson)
  if (!testing) {
    logger::log_messages()
    logger::log_warnings()
    logger::log_errors()
  }
}
