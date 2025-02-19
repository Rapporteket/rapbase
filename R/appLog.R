#' Settings for logging as json
#'
#' Every info, warning and error will be logged in json format.
#'
#' @param usernameEnv Global variable containing user name
#' @param appidEnv Global variable containing application name
#' @param hooks Logical defining if hooks for automatic logging should be set
#'
#' @export
#'
loggerSetup <- function(
  usernameEnv = "SHINYPROXY_USERNAME",
  appidEnv = "SHINYPROXY_APPID",
  hooks = TRUE
) {
  logger::log_threshold(logger::INFO)
  formatterJson <- function(level, message, ...) {
    username <- Sys.getenv(usernameEnv, unset = "unknown")
    appid <- Sys.getenv(appidEnv, unset = "unknown")
    jsonlite::toJSON(
      list(
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
        level = attr(level, "level"),
        app = appid,
        user = username,
        message = message
      ),
      auto_unbox = TRUE
    )
  }
  logger::log_layout(formatterJson)
  if (hooks) {
    logger::log_messages()
    logger::log_warnings()
    logger::log_errors()
  }
}

#' Wrapper around logger::log_shiny_input_changes
#'
#' @param ... Arguments passed to logger::log_shiny_input_changes function
#'
#' @export
#'
logShinyInputChanges <- function(...) {
  logger::log_shiny_input_changes(...)
}
