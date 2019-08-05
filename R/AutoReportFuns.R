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
#' @param fun String providing name of function to be called for generating
#' report
#' @param paramNames String vector where each element corresponds to the input
#' parameter to be used in the above function
#' @param paramValues String vector with corresponding values to paramNames
#' @param owner String providing the owner of the report. Usually a user name
#' @param email String with email address to recipient of email containing the
#' report
#' @param organization String identifying the organization the owner belongs to
#' @param runDayOfYear Integer vector with day numbers of the year when the
#' report is to be run
#' @param dryRun Logical defining if global auto report config actually is to
#' be updated. If set to TRUE the actual config (all of it) will be returned by
#' the function. FALSE by default
#'
#' @return Nothing unless dryRun is set TRUE in which case a list of all config
#' will be returned
#' @seealso \code{\link{deleteAutoReport}}
#' @export

createAutoReport <- function(synopsis, package, fun, paramNames, paramValues,
                             owner, email, organization, runDayOfYear,
                             dryRun = FALSE) {
  
  # make unique id by (hashing) combination of owner and timestamp
  ts <- as.character(as.integer(as.POSIXct(Sys.time())))
  autoRepId <- digest::digest(paste0(owner, ts))
  
  # make current entry, first named list of param names and values pairs
  l <- list()
  params <- paramValues
  names(params) <- paramNames
  paramsListVector <- list()
  for (i in 1:length(params)){
    paramsListVector[[i]] <- as.list(params[i])
  }
  
  l$synopsis <- synopsis
  l$package <- package
  l$fun <- fun
  l$params <- paramsListVector
  l$owner <- owner
  l$email <- email
  l$organization <- organization
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

readAutoReportData <- function(fileName = "autoReport.yml", packageName = "rapbase") {
  
  path <- Sys.getenv("R_RAP_CONFIG_PATH")
  
  if (path == "") {
    stopifnot(file.exists(system.file(fileName, package = packageName)))
    config_file <- system.file(fileName, package = packageName)
  } else {
    if (!file.exists(file.path(path, fileName))) {
      warning(paste("No configuration file found in", path,
                    ". A new file will be made from the package default"))
      file.copy(system.file(fileName, package = packageName), path)
    }
    config_file <- file.path(path, fileName)
  }
  
  yaml::yaml.load_file(config_file)
  
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
#' writeAutoReportData(config = config)
#' }

writeAutoReportData <- function(fileName = "autoReport.yml", config,
                                packageName = "rapbase") {
  
  path <- Sys.getenv("R_RAP_CONFIG_PATH")
  
  if (path == "") {
    # cannot proceed if there is nowhere to store config
    stop(paste("There is nowhere to store config data.", 
               "The environment variable R_RAP_CONFIG_PATH must be defined",
               "providing av path to a directory where configuration can",
               "be written. Stopping"))
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
    file.copy(from = oriFile, to = file.path(bckFilePath, bckFileName), overwrite = TRUE)
    # to maintain some order, remove files older than 30 days
    files <- file.info(list.files(bckFilePath, full.names = TRUE))
    rmFiles <- rownames(files[difftime(Sys.time(), files[, "mtime"],
                                       units = "days") > 30, ])
    file.remove(rmFiles)
    con <- file(oriFile, "w")
  }
  yaml::write_yaml(config, con)
  close(con)
}


#' Select data on one registry from config (list)
#'
#' Pick all config corresponding to a given registry. Registry name is not
#' given as such, but rather as its corresponding R package name. Hence, a
#' registry must be given as the name of its R package
#'
#' @param config list of configuration for automated reports
#' @param reg string giving the exact name of the R package for the registry
#'
#' @return list with config for registry reg
#' @export

selectByReg <- function(config, reg) {
  
  if (length(config) == 0 ) {
    list()
  } else {
    ind <- integer()
    for (i in 1:length(config)) {
      if (config[[i]]$package == reg) {
        ind <- c(ind, i)
      }
    }
    c(config[ind])
  }
}


#' Select data on one owner from config (list)
#'
#' Pick all config corresponding to a given owner (of the report)
#'
#' @param config list of configuration for automated reports
#' @param owner string giving the exact name owner
#'
#' @return list with config for reports belonging to owner
#' @export

selectByOwner <- function(config, owner) {
  
  if (length(config) == 0) {
    list()
  } else {
    ind <- integer()
    for (i in 1:length(config)) {
      if (config[[i]]$owner == owner) {
        ind <- c(ind, i)
      }
    }
    c(config[ind])
  }
}


#' Select data on one organization from config (list)
#'
#' Pick all config corresponding to a given organization (of the report)
#'
#' @param config list of configuration for automated reports
#' @param organization string giving the exact organization
#'
#' @return list with config for reports belonging to organization
#' @export

selectByOrganization <- function(config, organization) {
  
  if (length(config) == 0) {
    list()
  } else {
    ind <- integer()
    for (i in 1:length(config)) {
      if (config[[i]]$organization == organization) {
        ind <- c(ind, i)
      }
    }
    c(config[ind])
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
  for (i in 1:length(config)) {
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
#'
#' @return A simple message listing the contents of the arguments
#' @export
#'
#' @examples
#' .testAutoReport()

.testAutoReport <- function(aNum = 1, aChar = "a", anExp = Sys.Date()) {
  
  msg <- paste("This is a simple test of automated reports. Arguments provided:\n",
               "aNum:", as.character(aNum), ",\n",
               "aChar:", aChar, ",\n",
               "anExp:", as.character(anExp), "\n")
  fileName <- paste0(tempfile(), ".txt")
  con <- file(fileName, "w")
  cat(msg, file = fileName)
  close(con)
  
  fileName
  
}


#' Provide explicit reference to function for do.call
#'
#' @param x string with explicit reference, i.e. 'package::function'
#'
#' @return value of the exported 'function' in 'package'
#' @export

.getFun <- function(x) {
  
  if(length(grep("::", x))>0) {
    parts<-strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else { # nocov start
    x
  } # nocov end
  
}


#' Run reports as defined in yaml config and ship content by email
#'
#' Usually to be called by a scheduler, e.g. cron. If the provided day of
#' year matches those of the config the report is run as otherwise specified in
#' config. Functions called upon are expected to return a path to a file that
#' can be attached to an email. The email itself is defined and sent to
#' recipients defined in the config
#'
#' @param dayNumber Integer day of year where January 1st is 1. Defaults to
#' current day, i.e. as.POSIXlt(Sys.Date())$yday+1 (POSIXlt yday is base 0)
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

runAutoReport <- function(dayNumber = as.POSIXlt(Sys.Date())$yday+1,
                          dryRun = FALSE) {
  
    
  # get report candidates
  reps <- readAutoReportData()
  
  # standard text for email body
  stdTxt <- readr::read_file(system.file("autoReportStandardEmailText.txt",
                                         package = "rapbase"))
  
  for (i in 1:length(reps)) {
    rep <- reps[[i]]
    # get explicit referenced function
    f <- .getFun(paste0(rep$package, "::", rep$fun))
    if (dayNumber %in% rep$runDayOfYear) {
      attFile <- do.call(what = f, args = rep$params)
      if (dryRun) {
        message(paste("No emails sent. Attachment is", attFile))
      } else { # nocov start
        # get config
        conf <- rapbase::getConfig("rapbaseConfig.yml")
        # prepare email
        from <- conf$network$sender
        # escape spaces (e.g. when full name is added to <email>)
        to <- gsub(" ", "\\ ", rep$email, fixed = TRUE)
        subject <- rep$synopsis
        body <- list(stdTxt, sendmailR::mime_part(attFile))
        # ship the shite
        sendmailR::sendmail(
          from, to, subject, body,
          control = list(smtpServer=conf$network$smtp$server,
                         smtpPortSMTP=conf$network$smtp$port))
        
      } # nocov end
    }
  }
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
#'
makeRunDayOfYearSequence <- function(startDay = Sys.Date(), interval) {
  
  # set end to a year from start
  start <- as.POSIXlt(startDay)
  end <- start
  end$year <- end$year + 1
  s <- seq(from = start, to = end, by = interval)
  # skip redundant end value
  if (length(s) > 1) {
    s <- s[1:length(s)-1]
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
                            baseDayNum = as.POSIXlt(Sys.Date())$yday+1,
                            returnFormat = "%A %d. %B %Y") {
  
  year <- as.POSIXlt(Sys.Date())$year + 1900
  
  if (baseDayNum >= max(runDayOfYear) | length(runDayOfYear) == 1 &
      baseDayNum >= max(runDayOfYear)) {
    # next run will be first run of next year
    nextDayNum <- min(runDayOfYear)
    year <- year + 1
  } else {
    # next run will be next run day this year
    nextDayNum <- min(runDayOfYear[runDayOfYear > baseDayNum])
  }
  
  format(strptime(paste(year, nextDayNum), "%Y %j"), format = returnFormat)
  
}


#' Make table of subscriptions of reports
#' 
#' Make a table to be rendered in a shiny app providing the active
#' subscriptions of a given user within a given registry which are
#' both collected from the shiny session object provided
#'
#' @param session A shiny session object
#'
#' @return Matrix providing a table to be rendered in a shiny app
#' @importFrom magrittr "%>%"
#' @export

makeUserSubscriptionTab <- function(session) {
  
  . <- ""
  
  l <- list()
  autoRep <- readAutoReportData() %>%
    selectByReg(., reg = getUserGroups(session)) %>%
    selectByOwner(., owner = getUserName(session)) %>% 
    selectByOrganization(., organization = getUserReshId(session))
  
  for (n in names(autoRep)){
    r <- list("Rapport"=autoRep[[n]]$synopsis,
              "Enhet"=autoRep[[n]]$organization,
              "Neste"=findNextRunDate(autoRep[[n]]$runDayOfYear),
              "Slett"=as.character(
                shiny::actionButton(inputId = paste0("del_", n),
                             label = "",
                             icon = shiny::icon("trash"),
                             onclick = 'Shiny.onInputChange(\"del_button\",
                             this.id)')))
    l <- rbind(l, r)
  }
  l
}