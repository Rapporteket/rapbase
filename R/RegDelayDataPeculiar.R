#' Provide registration delay data from quality registries other than OpenQReg
#'
#' Provides delay between event (main date) and registration as a data frame.
#' This function handles each of the defined registries separately.
#'
#' @param registryName String Key/name of registry to get data from
#' @format Return a data frame with two variables:
#' \describe{
#' \item{year}{the year for the event}
#' \item{daysDiff}{the difference in days between event and the last time the
#' registration form was saved (preferably)}
#' }
#'
#' @return regDelayData data frame
#' @seealso This function takes care of those registries than can not be
#' handled by \code{\link{RegDelayData}}.
#' @export


RegDelayDataPeculiar <- function(registryName) {
  
  dbType <- "mysql"
  
  if (registryName == "nir") {
    query <- "
SELECT
  YEAR(DateAdmittedIntensive) as year,
  DATEDIFF(LastUpdate, DateDischargedIntensive) AS daysDiff
FROM
  Main"
  } else if (registryName == "Hjerneslag") {
    query <- "
    
    "
  } else if (registryName == "NorScir") {
    query <- "
    
    "
  } else if (registryName == "nkr") {
    dbType <- "mssql"
    query <- "
    
    "
  } else {
    stop("\nThe registry name provided is not a valid one.\n")
  }

  regDelayData <- rapbase::LoadRegData(registryName, query, dbType)
  
  return(regDelayData)
}
