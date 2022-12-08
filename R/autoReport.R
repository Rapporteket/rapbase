## Handle automated reports in config

#' Create and add report to config
#'
#' Adds an entry to the system configuration of reports to run at given
#' intervals. After generating the configuration from the new entry
#' the function load the current system configuration, adds the new
#' entry and saves the updated system configuration.
#'
#' @param synopsis String with description of the report and to be used in
#' subject field of email distributed reports
#' @param package String with package name also corresponding to registry
#' @param type Character string defining type of auto report. Currently, one of
#' 'subscription' (default) or 'dispatchment'
#' @param fun String providing name of function to be called for generating
#' report
#' @param paramNames String vector where each element corresponds to the input
#' parameter to be used in the above function
#' @param paramValues String vector with corresponding values to paramNames
#' @param owner String providing the owner of the report. Usually a user name
#' @param ownerName String providing full name of owner. Defaults to an empty
#' string to maintain backwards compatibility
#' @param email String with email address to recipient of email containing the
#' report
#' @param organization String identifying the organization the owner belongs to
#' @param runDayOfYear Integer vector with day numbers of the year when the
#' report is to be run
#' @param startDate Date-class date when report will be run first time. Default
#' value is set to \code{Sys.Date() + 1} \emph{i.e.} tomorrow.
#' @param terminateDate Date-class date after which report is no longer run.
#' Default value set to \code{NULL} in which case the function will provide an
#' expiry date adding 3 years to the current date if in a PRODUCTION context
#' and 1 month if not
#' @param interval String defining a time interval as defined in
#' \code{\link[base:seq.POSIXt]{seq.POSIXt}}. Default value is an empty string
#' @param intervalName String providing a human understandable representation of
#' \code{interval}. Default value is an empty string
#' @param dryRun Logical defining if global auto report config actually is to
#' be updated. If set to TRUE the actual config (all of it) will be returned by
#' the function. FALSE by default
#'
#' @return Nothing unless dryRun is set TRUE in which case a list of all config
#' will be returned
#' @seealso \code{\link{deleteAutoReport}}
#' @export

createAutoReport <- function(synopsis, package, type = "subscription", fun,
                             paramNames, paramValues, owner, ownerName = "",
                             email, organization, runDayOfYear,
                             startDate = as.character(Sys.Date()),
                             terminateDate = NULL, interval = "",
                             intervalName = "", dryRun = FALSE) {

  # When NULL, set expiry date based on context
  if (is.null(terminateDate)) {
    context <- Sys.getenv("R_RAP_INSTANCE")
    terminateDate <- as.POSIXlt(Sys.Date())
    if (context %in% c("PRODUCTION", "PRODUCTIONC")) {
      terminateDate$year <- terminateDate$year + 3
    } else {
      terminateDate$mon <- terminateDate$mon + 1
    }
    terminateDate <- as.Date(terminateDate)
  }

  # make unique id by (hashing) combination of owner and timestamp
  ts <- as.character(as.integer(as.POSIXct(Sys.time())))
  autoRepId <- digest::digest(paste0(owner, ts))

  l <- list()

  l$synopsis <- synopsis
  l$package <- package
  l$type <- type
  l$fun <- fun
  l$params <- as.list(stats::setNames(paramValues, paramNames))
  l$owner <- owner
  l$ownerName <- ownerName
  l$email <- email
  l$organization <- organization
  l$startDate <- as.character(startDate)
  l$terminateDate <- as.character(terminateDate)
  l$interval <- interval
  l$intervalName <- intervalName
  l$runDayOfYear <- runDayOfYear

  rd <- readAutoReportData()

  rd[[eval(autoRepId)]] <- l

  if (dryRun) {
    rd
  } else {
    writeAutoReportData(config = rd)
  }
}

#' Delete existing report from config
#'
#' @param autoReportId String providing the auto report unique id
#'
#' @seealso \code{\link{createAutoReport}}
#' @export

deleteAutoReport <- function(autoReportId) {
  rd <- readAutoReportData()
  # just stop with an error if report does not exist
  stopifnot(!is.null(rd[[autoReportId]]))
  ind <- names(rd) == autoReportId
  rd <- rd[!ind]
  writeAutoReportData(config = rd)
}

#' Read automated report metadata
#'
#' @param fileName String defining name of the yaml configuration file. Default
#' 'autoReport.yml'
#' @param packageName String defining the package in which the above
#' configuration file resides. A configuration file within an R-package is
#' only used in case the environmental variable 'R_RAP_CONFIG_PATH' is not
#' defined (empty)
#'
#' @return a list of yaml data
#' @export
#'
#' @examples
#' readAutoReportData()
readAutoReportData <- function(fileName = "autoReport.yml",
                               packageName = "rapbase") {
  config <- getConfig(fileName = "rapbaseConfig.yml")

  target <- config$r$autoReport$target

  if (target == "db") {
    # RMariaDB does not seem to handle json well, so cast to string serverside
    query <- "SELECT CAST(j AS CHAR) AS j FROM autoreport;"
    res <- rapbase::loadRegData(config$r$autoReport$key, query)
    conf <- jsonlite::unserializeJSON(res$j)
  } else if (target == "file") {
    path <- Sys.getenv("R_RAP_CONFIG_PATH")

    if (path == "") {
      stopifnot(file.exists(system.file(fileName, package = packageName)))
      config_file <- system.file(fileName, package = packageName)
    } else {
      if (!file.exists(file.path(path, fileName))) {
        warning(paste(
          "No configuration file found in", path,
          ". A new file will be made from the package default"
        ))
        file.copy(system.file(fileName, package = packageName), path)
      }
      config_file <- file.path(path, fileName)
    }

    conf <- yaml::yaml.load_file(config_file)
  }

  upgradeAutoReportData(conf)
  # conf
}

#' Upgrade auto reports
#'
#' Upgrade auto report config as new features emerge. Currently, the type
#' definition is added and set to 'subscription' that historically has been
#' the only type used
#'
#' @param config List of auto report configuration
#'
#' @return List of (upgraded) auto report configuration
#' @export

upgradeAutoReportData <- function(config) {
  upgradeType <- FALSE
  upgradeOwnerName <- FALSE
  upgradeParams <- FALSE
  upgradeStartDate <- FALSE

  for (i in seq_len(length(config))) {
    rep <- config[[i]]
    if (!"type" %in% names(rep)) {
      upgradeType <- TRUE
      config[[i]]$type <- "subscription"
    }
    if (!"ownerName" %in% names(rep)) {
      upgradeOwnerName <- TRUE
      config[[i]]$ownerName <- ""
    }
    if ("params" %in% names(rep) && inherits(rep$params[[1]], "list")) {
      upgradeParams <- TRUE
      paramName <- vector()
      paramValue <- vector()
      for (j in seq_len(length(rep$params))) {
        paramName[j] <- names(rep$params[[j]])
        paramValue[j] <- rep$params[[j]]
      }
      config[[i]]$params <- as.list(stats::setNames(paramValue, paramName))
    }
    if (!"startDate" %in% names(rep)) {
      upgradeStartDate <- TRUE
      config[[i]]$startDate <- "1900-01-01"
    }
  }

  if (upgradeType) {
    message(paste(
      "Auto report data were upgraded:",
      "auto reports with no type defined now set to",
      "'subscription'."
    ))
  }
  if (upgradeOwnerName) {
    message(paste(
      "Auto report data were upgraded:",
      "auto reports with no owner name defined now set to",
      "an empty string."
    ))
  }
  if (upgradeParams) {
    message(paste(
      "Auto report data were upgraded:",
      "function params list un-nested. Please check that autor reports for",
      "registries are still working as expected."
    ))
  }
  if (upgradeStartDate) {
    message(paste(
      "Auto report data were upgraded:",
      "auto reports with no start date defined now set to 1900-01-01."
    ))
  }

  config
}


#' Write automated report metadata
#'
#' @inheritParams readAutoReportData
#' @param config a list of yaml configuration
#'
#' @return NULL
#' @export
#'
#' @examples
#' \donttest{
#' # Example depend on environment variable R_RAP_CONFIG_PATH being set
#' config <- readAutoReportData()
#' try(writeAutoReportData(config = config))
#' }
#'
writeAutoReportData <- function(fileName = "autoReport.yml", config,
                                packageName = "rapbase") {
  rc <- getConfig(fileName = "rapbaseConfig.yml")
  target <- rc$r$autoReport$target
  key <- rc$r$autoReport$key

  if (target == "db") {
    config <- jsonlite::serializeJSON(config)
    query <- paste0("UPDATE autoreport SET j = '", config, "';")
    con <- rapOpenDbConnection(key)$con
    DBI::dbExecute(con, query)
    rapCloseDbConnection(con)
  } else if (target == "file") {
    path <- Sys.getenv("R_RAP_CONFIG_PATH")

    if (path == "") {
      # cannot proceed if there is nowhere to store config
      stop(paste(
        "There is nowhere to store config data.",
        "The environment variable R_RAP_CONFIG_PATH must be defined",
        "providing av path to a directory where configuration can",
        "be written. Stopping"
      ))
    } else {
      oriFile <- file.path(path, fileName)
      # in case we screw-up, make a backup
      tmpTag <- as.character(as.integer(as.POSIXct(Sys.time())))
      nameParts <- strsplit(fileName, "[.]")[[1]]
      bckFileName <- paste0(nameParts[1], tmpTag, ".", nameParts[-1])
      bckFilePath <- file.path(path, "autoReportBackup")
      if (!dir.exists(bckFilePath)) {
        dir.create(bckFilePath)
      }
      file.copy(
        from = oriFile, to = file.path(bckFilePath, bckFileName),
        overwrite = TRUE
      )
      # to maintain some order, remove files older than 30 days
      files <- file.info(list.files(bckFilePath, full.names = TRUE))
      rmFiles <- rownames(files[difftime(Sys.time(), files[, "mtime"],
        units = "days"
      ) > 30, ])
      file.remove(rmFiles)
      con <- file(oriFile, "w")
    }
    yaml::write_yaml(config, con)
    close(con)
  } else {
    stop(paste0(
      "Target ", target, " is not supported. Auto report data not written!"
    ))
  }
}


#' Filter auto report data
#'
#' Generic function to filter various entities from auto report data
#'
#' @param data List (nested) specifying auto reports to be filtered. May be
#' obtained by \code{rapbase::getConfig(fileName = "autoReport.yml")}
#' @param by Character string defining the filtering entity and must be one of
#' \code{c("package", "type", "owner", "organization")}. The term 'package'
#' represents the registry name
#' @param pass Character vector defining the values of the filtering entity that
#' will allow reports to pass through the filter
#'
#' @return List of auto reports matching the filtering criteria
#' @export
#'
#' @examples
#' ar <- list(ar1 = list(type = "A"), ar2 = list(type = "B"))
#' filterAutoRep(ar, "type", "B") # ar2
#'
filterAutoRep <- function(data, by, pass) {
  stopifnot(by %in% c("package", "type", "owner", "organization"))

  if (length(data) == 0) {
    list()
  } else {
    ind <- integer()
    for (i in seq_len(length(data))) {
      if (data[[i]][[by]] %in% pass) {
        ind <- c(ind, i)
      }
    }
    c(data[ind])
  }
}


#' Provide vector of registries (\emph{i.e.} their R packages) in config
#'
#' @param config list of configuration for automated reports
#'
#' @return character vector of registry (package) names
#' @export

getRegs <- function(config) {
  regs <- vector(mode = "character")
  for (i in seq_len(length(config))) {
    reg <- config[[i]]$package
    if (!(reg %in% regs)) {
      regs <- c(regs, reg)
    }
  }
  regs
}


## Run automated reports

#' Simple test of automated report
#'
#' Simple test of automated reporting from definitions provided in a yaml
#' config file
#'
#' @param aNum a number
#' @param aChar a character
#' @param anExp an expression
#' @param bulletin Integer defining if report is of type bulletin (1) or not
#' (0). Set to 0 by default
#'
#' @return A simple message listing the contents of the arguments
#' @export
#'
#' @examples
#' .testAutoReport()
.testAutoReport <- function(aNum = 1, aChar = "a", anExp = Sys.Date(),
                            bulletin = 0) {
  if (bulletin == 0) {
    bulletin <- FALSE
  } else {
    bulletin <- TRUE
  }

  msg <- paste(
    "This is a simple test of automated reports.",
    "Arguments provided:\n",
    "aNum:", as.character(aNum), ",\n",
    "aChar:", aChar, ",\n",
    "anExp:", as.character(anExp), "\n"
  )
  fileName <- paste0(tempfile(), ".txt")
  con <- file(fileName, "w")
  cat(msg, file = fileName)
  close(con)

  if (bulletin) {
    msg
  } else {
    fileName
  }
}


#' Provide explicit reference to function for do.call
#'
#' @param x string with explicit reference, i.e. 'package::function'
#'
#' @return value of the exported 'function' in 'package'
#' @export

.getFun <- function(x) {
  if (length(grep("::", x)) > 0) {
    parts <- strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else { # nocov start
    x
  } # nocov end
}


#' Run reports as defined in yaml config and ship content by email
#'
#' Usually to be called by a scheduler, e.g. cron. If the provided day of
#' year matches those of the config the report is run as otherwise specified in
#' config. Functions called upon are expected to return a character string
#' providing a path to a file that can be attached to an email or, in case of a
#' bulletin, the email body itself. For bulletins, files cannot be attached.
#' The email itself is prepared and sent to recipients defined in the config
#'
#' @param dayNumber Integer day of year where January 1st is 1. Defaults to
#' current day, \emph{i.e.} \code{as.POSIXlt(Sys.Date())$yday + 1} (POSIXlt
#' yday is base 0)
#' @param type Character vector defining the type of reports to be processed.
#' May contain one or more of
#' \code{c("subscription", "dispatchment", "bulletin")}. Defaults value set to
#' \code{c("subscription", "dispatchment")}.
#' @param dryRun Logical defining if emails are to be sent. If TRUE a message
#' with reference to the payload file is given but no emails will actually be
#' sent. Default is FALSE
#'
#' @return Emails with corresponding file attachment. If dryRun == TRUE just a
#' message
#' @export
#'
#' @examples
#' \donttest{
#' # Example depend on environment variable R_RAP_CONFIG_PATH being set
#' runAutoReport()
#' }
#'
runAutoReport <- function(dayNumber = as.POSIXlt(Sys.Date())$yday + 1,
                          type = c("subscription", "dispatchment"),
                          dryRun = FALSE) {
  . <- ""

  # get report candidates
  reps <- readAutoReportData() %>%
    filterAutoRep(., by = "type", pass = type)

  # standard text for email body
  stdTxt <- readr::read_file(system.file("autoReportStandardEmailText.txt",
    package = "rapbase"
  ))
  # get sender from common config
  conf <- rapbase::getConfig("rapbaseConfig.yml")

  for (i in seq_len(length(reps))) {
    tryCatch(
      {
        rep <- reps[[i]]
        if (dayNumber %in% rep$runDayOfYear &
          as.Date(rep$terminateDate) > Sys.Date() &
          as.Date(rep$startDate) <= Sys.Date()) {
          # get explicit referenced function and call it
          f <- .getFun(paste0(rep$package, "::", rep$fun))
          content <- do.call(what = f, args = rep$params)
          if (rep$type == "bulletin") {
            text <- content
            attFile <- NULL
          } else {
            text <- stdTxt
            attFile <- content
          }
          if (dryRun) {
            message(paste("No emails sent. Content is:", content))
          } else {
            autLogger(
              user = rep$owner,
              name = rep$ownerName,
              registryName = rep$package,
              reshId = rep$organization,
              type = rep$type,
              pkg = rep$package,
              fun = rep$fun,
              param = rep$params,
              msg = paste("recipients:", paste(rep$email,
                collapse = ", "
              ))
            )
            sendEmail(
              conf = conf, to = rep$email, subject = rep$synopsis,
              text = text, attFile = attFile
            )
          }
        }
      },
      error = function(e) {
        message(paste(
          "Report could not be processed (moving on to the next):",
          e
        ))
      }
    )
  }
}

#' Run bulletin auto reports
#'
#' This is a wrapper for \code{runAutoReport()} to issue bulletins. Purpose is
#' to ease simplify fire-in-the-hole at Rapporteket
#'
#' @return  Whatever \code{runAutoReport()} might provide
#' @export

runBulletin <- function() {
  runAutoReport(type = c("bulletin"))
}

## Miscellaneous functions

#' Make a sequence of day numbers from av given date and interval
#'
#' This function provides an even sequence of day numbers spanning 365/366
#' days from the start date and interval provided. Mainly to be used in
#' setting up automated reports at Rapporteket
#'
#' @param startDay Start date of sequence. May be provided as a string,
#' \emph{e.g.} \"2019-03-17\" or as class \"Date\". Defaults to today
#' @param interval String representing a valid seq.POSIXt interval such as
#' "DSTday", "week", "month", "quarter" or "year")
#'
#' @return Integer vector of day numbers
#' @export
#'
#' @examples
#' makeRunDayOfYearSequence(interval = "month")
makeRunDayOfYearSequence <- function(startDay = Sys.Date(), interval) {

  # set end to a year from start
  start <- as.POSIXlt(startDay)
  end <- start
  end$year <- end$year + 1
  s <- seq(from = start, to = end, by = interval)
  # skip redundant end value
  if (length(s) > 1) {
    s <- s[seq_len(length(s)) - 1]
  }
  unique(as.integer(format(s, "%j")))
}


#' Find next run date for automated reports
#'
#' Find the next date that an automated report is supposed to be run. Likely,
#' this function will only be relevant for shiny apps when this date is to
#' be printed
#'
#' @param runDayOfYear Numeric vector providing year-day numbers
#' @param baseDayNum Numeric defining base year-day. Default is today
#' @param startDate Character string of format "YYYY-MM-DD" defining the date of
#' the very first run. If set to NULL (default) or a none future date (compared
#' to the date represented by \code{baseDayNum} for the current year) it will be
#' disregarded.
#' @param returnFormat String providing return format as described in
#' \code{\link[base]{strptime}} in the current locale. Defaults to
#' "\%A \%d. \%B \%Y" that will provide something like
#' 'Mandag 20. januar 2019' in a Norwegian locale
#' @return String date for printing
#' @examples
#' # Will return Jan 30 in the current year and locale with default formatting
#' findNextRunDate(c(10, 20, 30), 20)
#' @export

findNextRunDate <- function(runDayOfYear,
                            baseDayNum = as.POSIXlt(Sys.Date())$yday + 1,
                            startDate = NULL,
                            returnFormat = "%A %e. %B %Y") {
  year <- as.POSIXlt(Sys.Date())$year + 1900

  if (!is.null(startDate)) {
    if (as.Date(startDate) > as.Date(strptime(
      paste(year, baseDayNum),
      "%Y %j"
    ))) {
      # since we pull the NEXT run day set new base day on day BEFORE star date
      baseDayNum <- as.POSIXlt(startDate)$yday
    }
  }

  # special case if out of max range and only one run day defined (yearly)
  if (baseDayNum >= max(runDayOfYear) || length(runDayOfYear) == 1) {
    # next run will be first run in day num vector
    nextDayNum <- min(runDayOfYear)
  } else {
    # find year transition, if any
    nDay <- length(runDayOfYear)
    deltaDay <- runDayOfYear[2:nDay] - runDayOfYear[1:(nDay - 1)]
    trans <- deltaDay < 0
    if (any(trans)) {
      indTrans <- match(TRUE, trans)
      # vector head
      dHead <- runDayOfYear[1:indTrans]
      # vector tail
      dTail <- runDayOfYear[(indTrans + 1):nDay]

      if (baseDayNum >= max(dTail)) {
        ## next run day to be found in vector head
        runDayOfYearSubset <- dHead
      } else {
        ## next run day to be found in vector tail
        runDayOfYearSubset <- dTail
      }
    } else {
      runDayOfYearSubset <- runDayOfYear
    }

    nextDayNum <- min(runDayOfYearSubset[runDayOfYearSubset > baseDayNum])
  }

  # if current day num larger than nextDayNum report will be run next year
  if (as.numeric(format(Sys.Date(), "%j")) > nextDayNum) {
    year <- year + 1
  }

  format(strptime(paste(year, nextDayNum), "%Y %j"), format = returnFormat)
}

# nolint start
#' Make table of automated reports
#'
#' Make a table to be rendered in a shiny app providing automated reports
#' from a given user or registry as obtained from the shiny session
#' object provided or environmental variables when run inside an app container.
#'
#' Each table record (line) represents a uniquely defined automated report.
#' For each line two shiny action buttons are provided to allow
#' for editing and deleting of each entry. For applications
#' implementing this table observing events on these action buttons may be used
#' to allow users to manage automated reports by GUI. The
#' action buttons for editing and deleting are provided with the static input
#' ids \emph{edit_button} and \emph{del_button} and upon clicking the
#' \emph{button} part of their ids will change to the unique id of the
#' report. Hence, a GUI call for editing a report can be caught by
#' \code{shiny::observeEvent("edit_button")} and within this event the
#' report id is obtained by collecting the string after the double underscore,
#' \emph{e.g.} \code{strsplit(input$edit_button, "__")[[1]][2]}.
#'
#' Optionally, report id may be provided as the last column in the table to
#' allow further development for registry specific purposes. Regardless, this
#' column should normally be hidden in the GUI.
#'
#' Take a look at the
#' \href{https://github.com/Rapporteket/rapRegTemplate/blob/main/inst/shinyApps/app1/server.R}{example shiny server function in rapRegTemplate}
#' on how this function may be implemented.
#'
#' @param session A shiny session object
#' @param namespace String naming namespace. Defaults to \code{character()} in
#'   which case no namespace will be created. When this function is used by
#'   shiny modules namespace must be provided.
#' @param user Character string providing the username. Introduced as a new
#'   argument when running apps inside containers. Default value is set to
#'   \code{rapbase::getUserName(session)} to allow backward compatibility.
#' @param group Character string defining the registry, normally corresponding
#'   to the R package name and the value stemming from the SHINYPROXY_GROUPS
#'   environment variable. Introduced as a new argument when running apps inside
#'   containers. Default value is set to \code{rapbase::getUserGroups(session)}
#'   to allow backward compatibility.
#' @param orgId Character string or integer defining the organization (id) for
#'   \code{user}. Default value is set to \code{rapbase::getUserReshId(session)}
#'   to allow backward compatibility.
#' @param type Character string defining the type of auto reports to tabulate.
#'   Must be one of \code{"subscription"}, \code{"dispatchment"} or
#'   \code{"bulletin"}. Default value set to \code{"subscription"}.
#' @param mapOrgId Data frame containing the two columns 'name' and 'id'
#'   corresponding to unique name and id of organizations. Default is NULL in
#'   which case the ids provided in auto report data will be used. In case
#'   mapOrgId is not NULL but no id match is found the id found in the auto
#'   report data will also be used
#' @param includeReportId Logical if the unique report id should be added as
#'   the last column in the table. FALSE by default.
#'
#' @return Matrix providing a table to be rendered in a shiny app
#' @importFrom magrittr "%>%"
#' @export
# nolint end

makeAutoReportTab <- function(session,
                              namespace = character(),
                              user = rapbase::getUserName(session),
                              group = rapbase::getUserGroups(session),
                              orgId = rapbase::getUserReshId(session),
                              type = "subscription",
                              mapOrgId = NULL,
                              includeReportId = FALSE) {
  stopifnot(type %in% c("subscription", "dispatchment", "bulletin"))

  . <- ""

  l <- list()
  autoRep <- readAutoReportData() %>%
    filterAutoRep(., by = "package", pass = group) %>%
    filterAutoRep(., by = "type", pass = type)

  if (type == "subscription") {
    autoRep <- autoRep %>%
      filterAutoRep(., by = "owner", pass = user) %>%
      filterAutoRep(., by = "organization", pass = orgId)
  }

  dateFormat <- "%A %e. %B %Y"

  for (n in names(autoRep)) {
    nextDate <- findNextRunDate(
      runDayOfYear = autoRep[[n]]$runDayOfYear,
      startDate = autoRep[[n]]$startDate,
      returnFormat = dateFormat
    )
    if (as.Date(nextDate, format = dateFormat) > autoRep[[n]]$terminateDate) {
      nextDate <- "Utl\u00F8pt"
    }
    dataSource <- autoRep[[n]]$organization
    if (!is.null(mapOrgId)) {
      if (dataSource %in% mapOrgId$id) {
        dataSource <- mapOrgId$name[mapOrgId$id == dataSource]
      }
    }
    r <- list(
      "Rapport" = autoRep[[n]]$synopsis,
      "Datakilde" = dataSource,
      "Mottaker" = paste0(autoRep[[n]]$email, collapse = "<br>"),
      "Periode" = autoRep[[n]]$intervalName,
      "Slutt" = strftime(as.Date(autoRep[[n]]$terminateDate),
        format = "%b %Y"
      ),
      "Neste" = nextDate,
      "Endre" = as.character(
        shiny::actionButton(
          inputId = shiny::NS(namespace, paste0("edit__", n)),
          label = "",
          icon = shiny::icon("edit"),
          onclick = sprintf(
            "Shiny.onInputChange('%s', this.id)",
            shiny::NS(namespace, "edit_button")
          )
        )
      ),
      "Slett" = as.character(
        shiny::actionButton(
          inputId = shiny::NS(namespace, paste0("del__", n)),
          label = "",
          icon = shiny::icon("trash"),
          onclick = sprintf(
            "Shiny.onInputChange('%s', this.id)",
            shiny::NS(namespace, "del_button")
          )
        )
      )
    )
    if (includeReportId) {
      r <- c(r, list("id" = n))
    }
    if (!type %in% c("subscription")) {
      r <- c(list(Ansvarlig = autoRep[[n]]$ownerName), r)
    }
    l <- rbind(l, r)
  }
  as.matrix(l)
}
