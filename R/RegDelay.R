#' Provide registration delay for OpenQReg quality registries
#'
#' Provide registration delay in median number of days (in addition to first
#' and third quartile, and N) grouped by years
#'
#' @param years integer vector with years for results and grouping
#' @inheritParams RegDelayData
#' @return data frame with registry name, quartiles 1, 2 and 3 and number of
#' observations for each year
#' @seealso Data to this function is provided by \code{\link{RegDelayData}}
#' and \code{\link{RegDelayDataPeculiar}}.
#' @export

RegDelay <- function(years, registryName, registrationFormName,
                     peculiarity = FALSE) {
  
  # get data
  if (peculiarity) {
    delayData <- RegDelayDataPeculiar(registryName)
  } else {
    delayData <- RegDelayData(registryName, registrationFormName)
  }
  
  # make data frame
  medianDelay <- data.frame(regName = registryName, stringsAsFactors = FALSE)
  sumDays <- 0
  for (i in years) {
    ind <- which(delayData$year == i)
    # find quartiles
    quartiles <- unname(quantile(delayData$daysDiff[ind]))
    medianDelay[[paste0("Q1", as.character(i))]] <- quartiles[2]
    medianDelay[[paste0("Q2", as.character(i))]] <- quartiles[3]
    medianDelay[[paste0("Q3", as.character(i))]] <- quartiles[4]
    medianDelay[[paste0("N", as.character(i))]] = length(ind)
    sumDays <- sumDays + quartiles[3]
  }
  medianDelay$sumDays <- sumDays
  
  return(medianDelay)
}
