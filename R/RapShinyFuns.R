#' Find next run date for automated reports
#' 
#' Find the next date that an automated report is supposed to be run. Likely,
#' this function will only be relevant for shiny apps when this date is to
#' be printed
#'
#' @param runDayOfYear Numeric vector providing year-day numbers  
#' @param baseDayNum Numeric defining base year-day. Default is today
#' @param returnFormat String providing return format as described in
#' base::strptime in the current locale. Defaults to "\%A \%d. \%B \%Y"
#' @return String date for printing
#' @examples
#' # Will return Jan 30 in the current year and locale with default formatting
#' findNextRunDate(c(10, 20, 30), 20)
#' @export

findNextRunDate <- function(runDayOfYear,
                            baseDayNum = as.POSIXlt(Sys.Date())$yday+1,
                            returnFormat = "%A %d. %B %Y") {
  
  year <- as.POSIXlt(Sys.Date())$year + 1900
  returnFormat <- "%A %d. %B %Y" #e.g. 'Mandag 20. januar 2019'
  
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
#' @export

makeUserSubscriptionTab <- function(session) {
  
  l <- list()
  autoRep <- readAutoReportData() %>%
    raptools::selectByReg(., reg = rapbase::getUserGroups(session)) %>%
    raptools::selectByOwner(., owner = rapbase::getUserName(session))
  
  for (n in names(autoRep)){
    r <- list("repId"=n,
              "Rapport"=autoRep[[n]]$synopsis,
              "Neste"=findNextRunDate(autoRep[[n]]$runDayOfYear),
              "Slett"=as.character(
                actionButton(inputId = paste0("del_", n),
                             label = "x",
                             onclick = 'Shiny.onInputChange(\"del_button\",  this.id)',
                             style = "color: red;")))
    l <- rbind(l, r)
  }
  l
}