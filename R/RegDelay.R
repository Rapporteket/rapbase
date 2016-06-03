#' Provide registration delay for OpenQReg quality registries
#'
#' Provide registration delay in median number of days grouped by years
#'
#' @param years integer vector with years for results and grouping
#' @inheritParams RegDelayData
#' @return data frame with registry name, median and number of observations
#' for each year
#' @seealso Data to this function is provided by \code{\link{RegDelayData}}.
#' @export

RegDelay <- function(years, registryName, registrationFormName) {
  
  # get data
  delayData <- RegDelayData(registryName, registrationFormName)
  
  # make data frame
  medianDelay <- data.frame(regName = registryName, stringsAsFactors = FALSE)
  for (i in years) {
    ind <- which(delayData$year == i)
    medianDelay[[as.character(i)]] = median(delayData$daysDiff[ind])
    medianDelay[[paste0("N", as.character(i))]] = length(ind)
  }
  
  return(medianDelay)
}
