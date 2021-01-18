#' Append a log record
#'
#' Append a record to the log. Normally, this function will not be called from
#' outside this package
#'
#' @param event data.frame of one record holding the fields of whatever that
#' is to be logged
#' @param name String defining the name of the log
#' @param target String defining where the log should go. Currently, 'file' is
#' the only option
#' @param format String defining the whatever format available given 'taget'.
#' Currently, the only option here is 'csv'
#'
#' @return Provides a new record in the log. If the log does not exist a new
#' one is created before appending the new record
#' @export
#'
#' @importFrom utils write.table

appendLog <- function(event, name, target, format) {

	if (target == "file") {
		path <- Sys.getenv("R_RAP_CONFIG_PATH")
		if (path == "") {
			stop(paste0("There is nowhere to append the logging event. ",
									"The environment variable R_RAP_CONFIG_PATH should be ",
									"defined!"))
		}
		name <- paste0(name, ".", format)
		if (format == "csv") {
			doAppend <- TRUE
			doColNames <- FALSE
			if (!file.exists(file.path(path, name)) ||
					file.size(file.path(path, name)) == 0) {
				doAppend <- FALSE
				doColNames <- TRUE
			}
			write.table(event, file = file.path(path, name), append = doAppend,
									col.names = doColNames, row.names = FALSE, sep = ",")
		}
	} else {
		stop(paste0("Target ", target, " is not supported. ",
								"Event was not appended!"))
	}

}


#' Make a log record
#'
#' Add default values and make a formatted log record
#'
#' @param content A named list of values to be logged
#' @param format String defining the format of the log record. Supported
#' values: 'csv' (default)
#'
#' @return A formatted log entry
#' @export
#'
#' @examples
#' makeLogRecord(list(msg="This is a test"))

makeLogRecord <- function(content, format = "csv") {

	defaultEntries <- list(
		time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
	)

	content <- c(defaultEntries, content)

	if (format == "csv") {
		as.data.frame(content)
	} else {
		stop(paste0("Format ", format, " is not supported. Event not logged!"))
	}

}

getSessionData <- function(session) {

	list(
		user = rapbase::getUserName(session),
		name = rapbase::getUserFullName(session),
		group = rapbase::getUserGroups(session),
		role = rapbase::getUserRole(session),
		resh_id = rapbase::getUserReshId(session)
	)
}

getSessionDataRep <- function(session) {

	# Currently not used
}


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
#'   provided by function argument (\code{subLogger()})
#'   \item \code{name}: full name of user as found in the shiny session object
#'   \item \code{group}: users group membership as provided by the shiny
#'     session object. Normally, this will correspond to the registry the user
#'     belongs to
#'   \item \code{role}: users role as provided by the shiny session object. Its
#'     value will depend on whatever is delivered by the autorization provider,
#'     but for OpenQReg registires 'LU' (local user) and 'SC' (system
#'     coordinator) are typical values
#'   \item \code{resh_id}: the organization id of the current user as provided
#'     by the shiny session object
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
#'     raplog::repLogger(session = list(...)[["session"]], msg = "Providing histogram")
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
#' @param .topcall Parent call (if any) calling this function. Used to provide
#' the function call with arguments. Default value is \code{sys.call(-1)}.
#' @param .topenv Name of the parent environment calling this function. Used to
#' provide package name (\emph{i.e.} register) this function was called from.
#' Default value is \code{parent.frame()}.
#'
#' @name logger
#' @aliases appLogger repLogger subLogger autLogger
#'
#' @return Returns nothing but calls a logging appender
NULL


#' @rdname logger
#' @export
#' @examples
#' \donttest{
#' # Depend on the environment variable R_RAP_CONFIG_PATH being set
#' appLogger(list())
#' }

appLogger <- function(session, msg = "No message provided") {

	name <- "appLog"
	content <- c(getSessionData(session), list(message=msg))
	event <- makeLogRecord(content, format = "csv")
	appendLog(event, name, target = "file", format = "csv")

}


#' @rdname logger
#' @export
#' @examples
#' \donttest{
#' # Depend on the environment variable R_RAP_CONFIG_PATH being set
#' repLogger(list())
#' }


repLogger <- function(session, msg = "No message provided",
											.topcall = sys.call(-1), .topenv = parent.frame()) {

	name <- "reportLog"
	parent_environment <- environmentName(topenv(.topenv))
	parent_call <- deparse(.topcall, width.cutoff = 160L, nlines = 1L)
	content <- c(getSessionData(session),
							 list(
							 	environment=parent_environment,
							 	call=parent_call,
							 	message=msg))
	event <- makeLogRecord(content, format = "csv")
	appendLog(event, name, target = "file", format = "csv")
}


#' @rdname logger
#' @export
#' @examples
#' \donttest{
#' # Depend on the environment variable R_RAP_CONFIG_PATH being set
#' autLogger(user = "ttester", registryName = "rapbase", reshId = "999999")
#' }

autLogger <- function(user, name, registryName, reshId, type,
											msg = "No message provided",
											.topenv = parent.frame()) {

	parent_environment <- environmentName(topenv(.topenv))
	content <- c(list(user = user,
										name = name,
										group = registryName,
										role = "NA",
										resh_id = reshId),
							 list(environment=parent_environment,
							 		 call = type,
							 		 message = msg))
	event <- makeLogRecord(content, format = "csv")
	appendLog(event, name = "reportLog", target = "file", format = "csv")
}
