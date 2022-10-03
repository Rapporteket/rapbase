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
    getSessionData(session, group = parent_environment),
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
    getSessionData(session),
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
        deparse(call(fun, unlist(param, recursive = FALSE)),
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
#' @return Provides a new record in the log. If the log does not exist a new
#' one is created before appending the new record when the log target is
#' configured to be files. When logging to a database this have to be set up in
#' advance.
#'
#' @keywords internal
#'
#' @importFrom utils write.table

appendLog <- function(event, name) {
  config <- getConfig(fileName = "rapbaseConfig.yml")
  target <- config$r$raplog$target

  if (target == "file") {
    path <- Sys.getenv("R_RAP_CONFIG_PATH")
    if (path == "") {
      stop(paste0(
        "There is nowhere to append the logfiles. ",
        "The environment variable R_RAP_CONFIG_PATH should be ",
        "defined!"
      ))
    }
    name <- paste0(name, ".csv")
    doAppend <- TRUE
    doColNames <- FALSE
    if (!file.exists(file.path(path, name)) ||
      file.size(file.path(path, name)) == 0) {
      doAppend <- FALSE
      doColNames <- TRUE
    }
    write.table(event,
      file = file.path(path, name), append = doAppend,
      col.names = doColNames, row.names = FALSE, sep = ","
    )
  } else if (target == "db") {
    con <- rapOpenDbConnection(config$r$raplog$key)$con
    DBI::dbAppendTable(con, name, event, row.names = NULL)
    rapCloseDbConnection(con)
  } else {
    stop(paste0(
      "Target ", target, " is not supported. ",
      "Log event was not appended!"
    ))
  }
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
#' @param session A shiny session object
#' @param group Character string providing the name of the app R package name.
#'   The term "group" is used to relate to the environmental variable
#'   SHINYPROXY_USERGROUPS that corresponds to the apps a given user can access.
#'
#' @return A list of relevant log fields
#' @keywords internal
getSessionData <- function(session, group = NULL) {
  list(
    user = rapbase::getUserName(session, group),
    name = rapbase::getUserFullName(session, group),
    group = rapbase::getUserGroups(session, group),
    role = rapbase::getUserRole(session, group),
    resh_id = rapbase::getUserReshId(session, group)
  )
}


#' Create a logging database
#'
#' Internal function that crates a database to be used for logging. Will return
#' an error if the database already exists.
#'
#' @param dbKey Character string with the key to a corresponding entry for the
#' database in dbConfig.yml. Please also make sure that the r.raplog.key
#' property in rapbaseConfig.yml is given the exact same value.
#'
#' @return Invisibly TRUE
#'
#' @keywords internal
createLogDb <- function(dbKey) {
  conf <- rapbase::getConfig()
  conf <- conf[[dbKey]]

  query <- paste0("CREATE DATABASE ", conf$name, ";")

  con <- RMariaDB::dbConnect(
    RMariaDB::MariaDB(),
    host = conf$host,
    user = conf$user,
    password = conf$pass
  )
  RMariaDB::dbExecute(con, query)
  RMariaDB::dbDisconnect(con)
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
  conf <- getConfig(fileName = "rapbaseConfig.yml")

  fc <- file(system.file("createRaplogTabs.sql", package = "rapbase"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries <- strsplit(sql, ";")[[1]]

  con <- rapOpenDbConnection(conf$r$raplog$key)$con
  for (i in seq_len(length(queries))) {
    RMariaDB::dbExecute(con, queries[i])
  }
  rapbase::rapCloseDbConnection(con)
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
#'
#' @return A data frame of log entries
#' @keywords internal
readLog <- function(type, name = "") {
  stopifnot(type == "report" | type == "app")

  config <- getConfig(fileName = "rapbaseConfig.yml")
  target <- config$r$raplog$target

  if (target == "file") {
    path <- Sys.getenv("R_RAP_CONFIG_PATH")

    if (path == "") {
      stop(paste(
        "No path to log-files provided. Make sure the environment",
        "varaible R_RAP_CONFIG_PATH is set!"
      ))
    }

    logFile <- switch(type,
      "report" = file.path(path, "reportLog.csv"),
      "app" = file.path(path, "appLog.csv")
    )
    if (!file.exists(logFile)) {
      stop(paste("Cannot find the log!", logFile, "does not exist."))
    }

    log <- utils::read.csv(logFile,
      header = TRUE,
      stringsAsFactors = FALSE
    )
    if (name != "") {
      log <- log %>%
        dplyr::filter(.data$group == !!name)
    }
  } else if (target == "db") {
    query <- paste0("SELECT * FROM ", type, "Log")
    if (name != "") {
      paste0(query, " WHERE group = ", name)
    }
    query <- paste0(query, ";")
    log <- loadRegData(config$r$raplog$key, query)
    log <- log %>%
      dplyr::select(-.data$id)
  } else {
    stop(paste0(
      "Log target '", target, "' is not supported. ",
      "Log could not be read! To remedy, please check that configuration is ",
      "set up properly."
    ))
  }

  invisible(log)
}

#' Sanitize log entries that have reached end of life
#'
#' @return NULL on success
#' @export
sanitizeLog <- function() {
  conf <- getConfig(fileName = "rapbaseConfig.yml")

  eolDate <- Sys.Date() - conf$r$raplog$eolDays

  if (conf$r$raplog$target == "file") {
    fileName <- c("appLog.csv", "reportLog.csv")
    logFile <- file.path(Sys.getenv("R_RAP_CONFIG_PATH"), fileName)
    backupDir <- file.path(
      Sys.getenv("R_RAP_CONFIG_PATH"),
      conf$r$raplog$archiveDir
    )
    backupFile <- file.path(backupDir, fileName)

    if (!dir.exists(backupDir)) {
      dir.create(backupDir)
    }

    for (i in seq_len(length(fileName))) {
      file.copy(logFile[i], backupFile[i], overwrite = TRUE)
      lf <- utils::read.csv(logFile[i])
      bf <- utils::read.csv(backupFile[i])
      backupOk <- digest::digest(lf) == digest::digest(bf)
      if (backupOk) {
        lf <- lf %>%
          dplyr::filter(as.Date(.data$time) > eolDate)
        write.table(
          lf,
          logFile[i],
          append = FALSE,
          sep = ",",
          row.names = FALSE,
          col.names = TRUE,
          qmethod = "double")
      }
    }
  }

  if (conf$r$raplog$target == "db") {
    con <- rapOpenDbConnection(conf$r$raplog$key)$con
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
  }

  NULL
}
