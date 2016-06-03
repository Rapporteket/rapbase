#' Make report data on registration delays across OpenQReg registries
#' 
#' @param years Integer Vector of years to group delays
#' @param registryKeyNames String Vector of registry names corresponding to the
#' key used for within registry packages
#' @param registrationFormNames String Vector of registrarion form names where
#' dates are to be collected from. Elements must correspond to
#' \emph{registryKeyNames}
#' @return regDelays Data frame of median delays
#' @seealso This function use \code{\link{RegDelay}}
#' @export

RegDelayReport <- function(years, registryKeyNames = "",
                           registrationFormNames = "") {
  
  # these are inputs, but hardcode for now
  registryKeyNames <- c("nger", "nger")
  registrationFormNames <- c("Operasjon", "Operasjon")
  
  for (i in (1:length(registryKeyNames))) {
    if (i == 1) {
      regDelays <- RegDelay(years = years, registryName = registryKeyNames[i],
                            registrationFormName = registrationFormNames[i])
    }
    else {
      regDelays <- rbind(regDelays,
                         RegDelay(years = years,
                                  registryName = registryKeyNames[i],
                                  registrationFormName = 
                                    registrationFormNames[i]))
    }
  }
  
  return(regDelays)
}