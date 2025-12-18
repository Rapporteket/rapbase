#' Log user events in shiny applications at Rapporteket
#'
#' To be used for logging at application level (\emph{i.e.} when a shiny
#' session is started) or at report level (\emph{i.e.} each time a report is
#' run). Logging of single report events should be made from reactive
#' environments within the shiny server function or from within the (report)
#' functions used by the same reactive environments.
#'
#' The below fields will be appended to the log, in the following order:
#' \enumerate{
#'   \item \code{time}: date-time as event is logged as
#'     \code{format(time, "\%Y-\%m-\%d \%H:\%M:\%S")}
#'   \item \code{user}: username as found in the shiny session object or as
#'   provided by function argument
#'   \item \code{name}: full name of user as found in the shiny session object
#'   \item \code{group}: users group membership as provided by the shiny
#'     session object. Normally, this will correspond to the registry the user
#'     belongs to
#'   \item \code{role}: users role as provided by the shiny session object. Its
#'     value will depend on whatever is delivered by the authorization provider,
#'     but for OpenQReg registries 'LU' (local user) and 'SC' (system
#'     coordinator) are typical values
#'   \item \code{resh_id}: the organization id of the current user as provided
#'     by the shiny session object, OR, when source of logging is auto reports,
#'     the organization ID of the data source used to make the report
#'   \item \code{environment}: environment from where the logger function was
#'     called (only provided by \code{repLogger()})
#'   \item \code{call}: function (with arguments) from where the logger was
#'     called (only provided by \code{repLogger()})
#'   \item message: an optional message defined as argument to the function
#' }
#'
#' The \code{autLogger()} function is a special case to be used for automated
#' reports. Since such reports are run outside a reactive (shiny) context
#' shiny session data are not available to the logger. Hence, logging data
#' must be provided as arguments directly. As of rapbase version 1.12.0 logging
#' of automated reports are already taken care of. Hence, this function should
#' not be applied per registry application.
#'
#' @note Pseudo code of how \code{appLogger()} may be implemented:
#' \preformatted{
#' library(shiny)
#' library(raplog)
#'
#' server <- function(input, output, session) {
#'   raplog::appLogger(session, msg = "Smerteregisteret: starting shiny app")
#'   ...
#' }
#' }
#' Pseudo code on how \code{repLogger()} can be implemented as part of a
#' function in a reactive (shiny) context. First, this is an example of the
#' shiny server function with the (reactive) function \code{renderPlot()}
#' calling a function that provides a histogram:
#' \preformatted{
#' library(shiny)
#' library(raplog)
#'
#' server <- function(input, output, session) {
#'   ...
#'   output$hist <- renderPlot({
#'     makeHist(data, var = input$var, bins = input$bins, session = session)
#'   })
#'   ...
#' }
#' } Then, logging is called within the function \code{makeHist()}:
#' \preformatted{
#' makeHist <- function(data, var, bins, ...) {
#'
#'   if ("session" \%in\% names(list(...))) {
#'     raplog::repLogger(session = list(...)[["session"]],
#'                       msg = "Providing histogram")
#'   }
#'   ...
#' }
#' }
#'
#' @param session Shiny session object to be used for getting user data.
#' For testing and development purposes \code{session} can be replaced by
#' \code{list()} in which case various config options might be used to provide
#' something sensible.
#' @param msg String providing a user defined message to be added to the log
#' record. Default value is 'No message provided'.
#' @param user String providing owner of an automated report. Its value should
#' correspond to the actual user name as provided in a shiny session at
#' Rapporteket. Only used for subscription reports that are run outside a shiny
#' session.
#' @param name String providing full name of the report owner. Only used for
#' automated reports that are run outside a shiny session.
#' @param registryName String providing registry name. Only used for automated
#' reports that are run outside a shiny session.
#' @param reshId String providing the organization id of the (subscription)
#' report author. Only used for automated reports that are run outside a shiny
#' session.
#' @param type Character string defining the type of report. Only used for
#' automated reports that are run outside a shiny session in which case its
#' value will replace that of \code{.topcall}.
#' @param pkg Character string naming the package of the function that is to be
#' logged. Only used for automated reports that are run outside a shiny
#' session.
#' @param fun Character string naming the function that should be logged. Only
#' used for automated reports that are run outside a shiny session.
#' @param param List of named function parameter. Only used for automated
#' reports that are run outside a shiny session.
#' @param .topcall Parent call (if any) calling this function. Used to provide
#' the function call with arguments. Default value is \code{sys.call(-1)}.
#' @param .topenv Name of the parent environment calling this function. Used to
#' provide package name (\emph{i.e.} register) this function was called from.
#' Default value is \code{parent.frame()}.
#'
#' @name logger
#' @aliases appLogger repLogger autLogger
#'
#' @return Returns nothing but calls a logging appender
NULL


#' @rdname logger
#' @export
#' @examples
#' \donttest{
#' # Depend on the environment variable R_RAP_CONFIG_PATH being set
#' try(appLogger(list()))
#' }
#'
appLogger <- function(session, msg = "No message provided",
                      .topcall = sys.call(-1), .topenv = parent.frame()) {
  name <- "appLog"
  parent_environment <- environmentName(topenv(.topenv))
  content <- c(
    getSessionData(group = parent_environment),
    list(message = msg)
  )
  event <- makeLogRecord(content)
  appendLog(event, name)
}


#' @rdname logger
#' @export
#' @examples
#' \donttest{
#' # Depend on the environment variable R_RAP_CONFIG_PATH being set
#' try(repLogger(list()))
#' }
#'
repLogger <- function(session, msg = "No message provided",
                      .topcall = sys.call(-1), .topenv = parent.frame()) {
  name <- "reportLog"
  parent_environment <- environmentName(topenv(.topenv))
  parent_call <- deparse(.topcall, width.cutoff = 160L, nlines = 1L)
  content <- c(
    getSessionData(),
    list(
      environment = parent_environment,
      call = parent_call,
      message = msg
    )
  )
  event <- makeLogRecord(content)
  appendLog(event, name)
}


#' @rdname logger
#' @export
#' @examples
#' \donttest{
#' # Depend on the environment variable R_RAP_CONFIG_PATH being set
#' try(autLogger(user = "ttester", registryName = "rapbase", reshId = "999999"))
#' }
#'
autLogger <- function(user, name, registryName, reshId, type, pkg, fun, param,
                      msg = "No message provided", .topenv = parent.frame()) {
  parent_environment <- environmentName(topenv(.topenv))
  content <- c(
    list(
      user = user,
      name = name,
      group = registryName,
      role = "NA",
      resh_id = reshId
    ),
    list(
      environment = parent_environment,
      call = paste0(
        pkg, "::",
        deparse(
          call(fun, unlist(param, recursive = FALSE)),
          width.cutoff = 500
        )
      ),
      message = paste(type, msg)
    )
  )
  event <- makeLogRecord(content)
  appendLog(event, name = "reportLog")
}


#' Append a log record
#'
#' Internal function adding a record to the log.
#'
#' @param event data.frame of one record holding the fields of whatever that
#' is to be logged.
#' @param name String defining the name of the log, currently one of "appLog" or
#' "reportLog".
#'
#' @return Provides a new record in the log database. The database have to be
#' set up in advance.
#'
#' @keywords internal
#'
#' @importFrom utils write.table

appendLog <- function(event, name) {
  tryCatch({
    con <- rapOpenDbConnection("raplog")$con
    DBI::dbAppendTable(con, name, event, row.names = NULL)
    rapCloseDbConnection(con)
    con <- NULL
  } , error = function(e) {
    warning(paste0(
      "Log entry could not be appended to '", name, "' log!\n",
      "Please check that configuration is set up properly.\n",
      "Original error message:\n", e$message, ".\n",
      "The message that was to be logged:\n", paste(event, collapse = "; ")
    ))
  })
}


#' Make a log record
#'
#' Internal function adding default values and make a formatted log record.
#'
#' @param content A named list of values to be logged
#'
#' @return A formatted log entry
#' @keywords internal
makeLogRecord <- function(content) {
  defaultEntries <- list(
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  content <- c(defaultEntries, content)

  as.data.frame(content)
}

#' Get session data
#'
#' Internal function providing session data relevant to logging.
#'
#' @param group Character string providing the name of the app R package name.
#'   The term "group" is used to relate to the environmental variable
#'   SHINYPROXY_USERGROUPS that corresponds to the apps a given user can access.
#'
#' @return A list of relevant log fields
#' @keywords internal
getSessionData <- function(group = NULL) {
  list(
    user = getUserName(group),
    name = getUserFullName(group),
    group = getUserGroups(group),
    role = getUserRole(group),
    resh_id = getUserReshId(group)
  )
}

#' Create tables for log entries in a database
#'
#' Internal function that crates a database tables to be used for logging. Will
#' return an error if the table(s) already exists.
#'
#' @return Invisibly TRUE
#'
#' @keywords internal
createLogDbTabs <- function() {

  fc <- file(system.file("createRaplogTabs.sql", package = "rapbase"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries <- strsplit(sql, ";")[[1]]

  con <- rapOpenDbConnection("raplog")$con
  for (i in seq_along(queries)) {
    DBI::dbExecute(con, queries[i])
  }
  rapCloseDbConnection(con)
  con <- NULL
}


#' Read log entries
#'
#' Internal function that provide log entries
#'
#' @param type Character string defining which log to request data from. Must be
#' one of \code{c("app", "report")}.
#' @param name Character string with registry filter. Default value is an empty
#' string that will return all log entries. If not empty its value must
#' correspond to an existing registry (\emph{i.e.} R package) name.
#' @param app_id An identifier for a particular registry. Default value is NULL,
#' in which case no action is taken. If value is provided, the log is filtered
#' to show only entries matching chosen app_id.
#'
#' @return A data frame of log entries
#' @keywords internal
readLog <- function(type, name = "", app_id = NULL) {
  stopifnot(type == "report" | type == "app")

  tryCatch({
    query <- paste0("SELECT * FROM ", type, "Log")
    query <- paste0(query, ";")
    log <- loadRegData("raplog", query)
    if (!is.null(app_id)) {
      log <- log[which(log$group == app_id), ]
    }
    log <- log |>
      dplyr::select(-"id")
    invisible(log)
  } , error = function(e) {
    warning(paste0(
      "Log could not be read! To remedy, please check that configuration is ",
      "set up properly.\nOriginal error message:\n", e$message
    ))
    invisible(NULL)
  })
}

#' Sanitize log entries that have reached end of life
#'
#' Function that removes log entries older than a given number of days.
#'
#' @param eolDays Number of days to keep log entries. Entries older than this
#' will be removed. Default value is 730 days (2 years).
#'
#' @return NULL on success
#' @export
sanitizeLog <- function(eolDays = 730) {
  tryCatch({
    eolDate <- Sys.Date() - eolDays

    con <- rapOpenDbConnection("raplog")$con
    query <- paste0(
      "DELETE FROM appLog WHERE time < '",
      as.character(eolDate), "';"
    )
    DBI::dbExecute(con, query)
    query <- paste0(
      "DELETE FROM reportLog WHERE time < '",
      as.character(eolDate), "';"
    )
    DBI::dbExecute(con, query)
    rapCloseDbConnection(con)
    con <- NULL
    NULL
  } , error = function(e) {
    warning(paste0(
      "Log could not be sanitized! ",
      "To remedy, please check that configuration is ",
      "set up properly."
    ))
  })
}
