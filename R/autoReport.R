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

createAutoReport <- function(
  synopsis,
  package,
  type = "subscription",
  fun,
  paramNames,
  paramValues,
  owner,
  ownerName = "",
  email,
  organization,
  runDayOfYear,
  startDate = as.character(Sys.Date()),
  terminateDate = NULL,
  interval = "",
  intervalName = "",
  dryRun = FALSE
) {

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

  l <- list()

  l$synopsis <- synopsis
  l$package <- package
  l$type <- type
  l$fun <- fun
  l$params <- as.list(stats::setNames(paramValues, paramNames))
  l$owner <- owner
  l$ownerName <- ownerName
  l$email <- email
  l$organization <- as.character(organization)
  l$startDate <- as.character(startDate)
  l$terminateDate <- as.character(terminateDate)
  l$interval <- interval
  l$intervalName <- intervalName
  l$runDayOfYear <- runDayOfYear

  rd <- list(l)

  if (dryRun) {
    rd
  } else {
    message(
      paste0(
        "Creating auto report with synopsis: ",
        synopsis,
        ", package: ",
        package,
        ", type: ",
        type,
        ", function: ",
        fun,
        ", owner: ",
        owner,
        ", email: ",
        email
      )
    )
    writeAutoReportData(config = rd)
  }
}

#' Delete existing report from config/db
#'
#' @param autoReportId String providing the auto report unique id
#'
#' @seealso \code{\link{createAutoReport}}
#' @export

deleteAutoReport <- function(autoReportId) {
  message(
    paste0(
      "Deleting auto report with id: ",
      autoReportId
    )
  )
  query <- paste0(
    "DELETE FROM autoreport ",
    " WHERE id = '",
    autoReportId,
    "';"
  )
  dbConnect <- rapOpenDbConnection("autoreport")
  DBI::dbExecute(dbConnect$con, query)
  rapCloseDbConnection(dbConnect$con)
  dbConnect <- NULL
}

#' Read automated report metadata
#'
#'
#' @return a table of autoreport data
#' @export
#'
#' @examples
#' \donttest{
#' try(readAutoReportData())
#' }
readAutoReportData <- function() {
  query <- paste0("SELECT * FROM autoreport;")
  res <- loadRegData("autoreport", query)
  res
}


#' Write automated report metadata
#'
#' @param config a list of yaml configuration
#'
#' @return NULL
#' @export
#'
#' @examples
#' \donttest{
#' # Example depend on environment variable R_RAP_CONFIG_PATH being set
#' try(config <- readAutoReportData())
#' try(writeAutoReportData(config = config))
#' }
#'
writeAutoReportData <- function(config) {
  # Create empty data frame
  dataframe <- stats::setNames(
    data.frame(
      matrix(ncol = 15, nrow = 0)
    ),
    c("id", names(config[[1]]))
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  for (element in config) {
    for (email in element$email) {
      dataframe <- dataframe |> dplyr::add_row(
        id = digest::digest(
          paste0(
            email,
            as.character(as.integer(as.POSIXct(Sys.time())))
          )
        ),
        synopsis = element$synopsis,
        package = element$package,
        fun = element$fun,
        params = paste0(
          "{",
          paste0(
            "\"",
            names(element$params),
            "\": \"",
            element$params,
            "\"",
            collapse = ", "
          ), "}"
        ),
        owner = element$owner,
        email = email,
        organization = element$organization,
        terminateDate = element$terminateDate,
        interval = element$interval,
        intervalName = element$intervalName,
        type = element$type,
        ownerName = element$ownerName,
        startDate = element$startDate,
        runDayOfYear = toString(element$runDayOfYear)
      )
    }
  }
  message(paste0("Add auto report data to database, ",
                 nrow(dataframe), " entries."))
  con <- rapOpenDbConnection("autoreport")$con
  DBI::dbAppendTable(con, "autoreport", dataframe, row.names = NULL)
  rapCloseDbConnection(con)
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
#' ar <- data.frame(type = c("A", "B"))
#' filterAutoRep(ar, "type", "B") # ar2
#'
filterAutoRep <- function(
  data,
  by,
  pass
) {
  stopifnot(by %in% c("package", "type", "owner", "organization"))

  if (length(data) == 0) {
    list()
  } else {
    dplyr::filter(data, .data[[by]] %in% pass)
  }
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
#' @param dato Date-class date when report will be run first time. Default value
#' is set to \code{Sys.Date()}
#' @inheritParams makeAutoReportTab
#'
#' @return Emails with corresponding file attachment. If dryRun == TRUE just a
#' message
#' @export
#'
#' @examples
#' \donttest{
#' # Example depend on environment variable R_RAP_CONFIG_PATH being set
#' try(runAutoReport())
#' }
#'

runAutoReport <- function(
  dayNumber = as.POSIXlt(Sys.Date())$yday + 1,
  dato = Sys.Date(),
  group = NULL,
  type = c("subscription", "dispatchment"),
  dryRun = FALSE
) {

  # get report candidates
  reps <- readAutoReportData() |>
    filterAutoRep(by = "type", pass = type)
  if (!is.null(group)) {
    reps <- reps |>
      filterAutoRep(by = "package", pass = group)
  }

  reps <- reps |>
    # keep only reports to be run today
    dplyr::rowwise() |>
    dplyr::filter(
      # nolint start: vector_logic_linter.
      # we need && here to ensure no crash if startDate < dato.
      # Thus, seq.Date will not be called if one of the first
      # two conditions is FALSE.
      (as.Date(.data$startDate) <= dato) &&
        (as.Date(.data$terminateDate) > dato) &&
        (dato %in% seq.Date(
          as.Date(.data$startDate),
          as.Date(dato),
          by = .data$interval
        ))
      # nolint end
    ) |>
    dplyr::ungroup() |>
    # combine emails for identical reports
    dplyr::summarise(
      email = list(unique(email)),
      .by = c(
        "owner",
        "ownerName",
        "package",
        "organization",
        "type",
        "fun",
        "params",
        "synopsis"
      )
    )

  if (dim(reps)[1] == 0) {
    message(
      "runAutoReport: No reports to be processed today (",
      dato,
      ifelse(is.null(group), ").", paste0(") from registry ", group, "."))
    )
    return(invisible(NULL))
  }

  # standard text for email body
  standardEmailFileName <- "autoReportStandardEmailText.txt"
  stdTxt <- readr::read_file(
    system.file(
      standardEmailFileName,
      package = "rapbase"
    )
  )
  # get sender from common config
  conf <- getConfig("rapbaseConfig.yml")

  message("runAutoReport: Starting processing of auto reports")
  for (i in seq_len(dim(reps)[1])) {
    message(paste0(
      "Processing report ", i, " of ", dim(reps)[1],
      " from package ", reps$package[i], ". Synopsis: ", reps$synopsis[i]
    ))
    tryCatch(
      {
        rep <- reps[i, ] |> as.list()
        rep$email <- unlist(rep$email)
        params <- jsonlite::fromJSON(rep$params)
        # get referenced function and call it
        if (grepl("::", rep$fun)) {
          # get explicit referenced function and call it
          f <- .getFun(rep$fun)
        } else {
          f <- .getFun(paste0(rep$package, "::", rep$fun))
        }
        content <- do.call(what = f, args = params)
        if (rep$type == "bulletin") {
          text <- content
          attFile <- NULL
        } else {
          attFile <- content
          if (file.exists(system.file(
            standardEmailFileName,
            package = rep$package
          ))) {
            # read standard text from package
            text <- readr::read_file(
              system.file(
                standardEmailFileName,
                package = rep$package
              )
            )
          } else {
            text <- stdTxt
          }
        }
        if (dryRun) {
          message(paste("No emails sent. Content is:", content))
        } else {
          for (email in rep$email) {
            message(paste(
              "Report", i, "of", dim(reps)[1],
              ". Sending email to:", email
            ))
            autLogger(
              user = rep$owner,
              name = rep$ownerName,
              registryName = rep$package,
              reshId = rep$organization,
              type = rep$type,
              pkg = rep$package,
              fun = rep$fun,
              param = rep$params,
              msg = paste(
                "recipient:",
                email
              )
            )
            sendEmail(
              conf = conf, to = email, subject = rep$synopsis,
              text = text, attFile = attFile
            )
          }
        }
      },
      error = function(e) {
        warning(paste(
          "Report could not be processed (moving on to the next):",
          e
        ))
      }
    )
  }
  message("runAutoReport: Finished processing of auto reports")
}

#' Run bulletin auto reports
#'
#' This is a wrapper for \code{runAutoReport()} to issue bulletins.
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
    s <- s[seq_along(s) - 1]
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
#' @inheritParams createAutoReport
#' @return String date for printing
#' @examples
#' # Will return Jan 30 in the current year and locale with default formatting
#' findNextRunDate(runDayOfYear = c(10, 20, 30),
#'  baseDayNum = 20, startDate = 1, terminateDate = 50)
#' @export

findNextRunDate <- function(
  runDayOfYear,
  baseDayNum = as.POSIXlt(Sys.Date())$yday + 1,
  startDate,
  terminateDate,
  interval = NULL,
  returnFormat = "%A %e. %B %Y"
) {

  if (Sys.Date() < startDate) {
    nextDate <- as.Date(startDate)
  }
  if (Sys.Date() >= startDate && Sys.Date() <= terminateDate) {
    dateseq <- seq.Date(
      as.Date(startDate),
      as.Date(terminateDate),
      by = interval
    )
    tidsdiff <- difftime(dateseq, Sys.Date(), units = "days")
    tidsdiff[tidsdiff <= 0] <- NA
    if (length(tidsdiff) == sum(is.na(tidsdiff))) {
      nextDate <- as.Date(terminateDate) + 1
    } else {
      nextDate <- dateseq[which(tidsdiff == min(tidsdiff, na.rm = TRUE))]
    }
  }
  if (Sys.Date() > terminateDate) {
    nextDate <- as.Date(terminateDate) + 1
  }
  return(format(nextDate, format = returnFormat))
}

# nolint start
#' Make table of automated reports
#'
#' Make a table to be rendered in a shiny app providing automated reports
#' from a given user or registry as obtained from the environmental variables.
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
#' @param namespace String naming namespace. Defaults to \code{character()} in
#'   which case no namespace will be created. When this function is used by
#'   shiny modules namespace must be provided.
#' @param user Character string providing the username. Introduced as a new
#'   argument when running apps inside containers. Default value is set to
#'   \code{rapbase::getUserName()} to allow backward compatibility.
#' @param group Character string defining the registry, normally corresponding
#'   to the R package name and the value stemming from the SHINYPROXY_GROUPS
#'   environment variable. Introduced as a new argument when running apps inside
#'   containers. Default value is set to \code{rapbase::getUserGroups()}
#'   to allow backward compatibility.
#' @param orgId Character string or integer defining the organization (id) for
#'   \code{user}. Default value is set to \code{rapbase::getUserReshId()}
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
#' @export
# nolint end

makeAutoReportTab <- function(
  namespace = character(),
  user = getUserName(),
  group = getUserGroups(),
  orgId = getUserReshId(),
  type = "subscription",
  mapOrgId = NULL,
  includeReportId = FALSE
) {
  stopifnot(type %in% c("subscription", "dispatchment", "bulletin"))

  autoRep <- readAutoReportData() |>
    filterAutoRep(by = "package", pass = group) |>
    filterAutoRep(by = "type", pass = type)

  if (type == "subscription") {
    autoRep <- autoRep |>
      filterAutoRep(by = "owner", pass = user) |>
      filterAutoRep(by = "organization", pass = orgId)
  }

  dateFormat <- "%A %e. %B %Y"

  if (length(autoRep$id) == 0) {
    return(as.matrix(autoRep))
  }

  l <- list()
  for (i in seq_len(nrow(autoRep))) {
    nextDate <- findNextRunDate(
      runDayOfYear = NULL,
      startDate = autoRep[i, ]$startDate,
      terminateDate = autoRep[i, ]$terminateDate,
      interval = autoRep[i, ]$interval,
      returnFormat = dateFormat
    )
    if (as.Date(nextDate, format = dateFormat) > autoRep[i, ]$terminateDate) {
      nextDate <- "Utl\u00F8pt"
    }
    dataSource <- autoRep[i, ]$organization
    if (!is.null(mapOrgId)) {
      if (dataSource %in% mapOrgId$id) {
        dataSource <- mapOrgId$name[mapOrgId$id == dataSource]
      }
    }
    r <- list(
      "Rapport" = autoRep[i, ]$synopsis,
      "Datakilde" = dataSource,
      "Mottaker" = autoRep[i, ]$email,
      "Periode" = autoRep[i, ]$intervalName,
      "Slutt" = strftime(
        as.Date(
          autoRep[i, ]$terminateDate
        ),
        format = dateFormat
      ),
      "Neste" = nextDate,
      "Endre" = as.character(
        shiny::actionButton(
          inputId = shiny::NS(namespace, paste0("edit__", autoRep[i, ]$id)),
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
          inputId = shiny::NS(namespace, paste0("del__", autoRep[i, ]$id)),
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
      r <- c(r, list("id" = autoRep[i, ]$id))
    }
    if (!type %in% c("subscription")) {
      r <- c(list(Ansvarlig = autoRep[i, ]$ownerName), r)
    }
    l <- rbind(l, r)
  }
  return(as.matrix(l))
}
