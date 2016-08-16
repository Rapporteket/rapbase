#' Provide registration delay data from OpenQReg quality registries
#'
#' Provides delay between event (main date) and registration as a data frame
#'
#' @param registryName String Key/name of registry to get data from
#' @param registrationFormName String Name of the registry form where dates
#' are to be collected from
#' @format Return a data frame with two variables:
#' \describe{
#' \item{year}{the year for the event (from db field \emph{HovedDato})}
#' \item{daysDiff}{the difference in days between event (from db field
#' \emph{HovedDato}) and the last time the registration form was saved
#' (from db field \emph{SistLagretDato})}
#' }
#'
#' @details For the query these conditions apply:
#' \describe{
#' \item{SkjemaStatus = 1}{use Only finished registrations}
#' \item{SkjemaNavn = registrationFormName}{use only registry form name given
#' by the input argument \emph{registrationFormName}. For surgical registries
#' this is usually the form "Operasjon".}
#' }
#' @return regDelayData data frame
#' @seealso This function is used by \code{\link{RegDelay}}.
#' @export


RegDelayData <- function(registryName, registrationFormName) {
  
  dbType <- "mysql"
  
  # temporary fix discrepancy field name. Should be fixed at db-level for 'nger'
  registrationFormFieldName <- "Skjemanavn"
  if (registryName == "nger") {
    registrationFormFieldName <- "SkjemaNavn"
  }
  
  query <- paste0(
    'select
  year(HovedDato) as year,
  DATEDIFF(SistLagretDato, HovedDato) as daysDiff
from
  SkjemaOversikt
where
  SkjemaStatus=1 and ', registrationFormFieldName, '="',
    registrationFormName, '";'
  )
  
  regDelayData <- rapbase::LoadRegData(registryName, query, dbType)
  
  return(regDelayData)
}
