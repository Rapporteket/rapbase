#' Make report data on registration delays across OpenQReg registries
#' 
#' @param years Integer Vector of years to group delays
#' @param registryKeyNames String Vector of registry names corresponding to the
#' key used for within registry packages
#' @param registrationFormNames String Vector of registration form names where
#' dates are to be collected from. Elements must correspond to
#' \emph{registryKeyNames}
#' @param peculiarity Logical vector, TRUE for none-OpenQReg registries.
#' Elements must correspond to \emph{registryKeyNames}
#' @return regDelays Data frame of median delays
#' @seealso This function use \code{\link{RegDelay}}
#' @export

RegDelayReport <- function(years, registryKeyNames, registrationFormNames,
                           peculiarity) {
  
  # these are inputs, but hardcode for now
  #registryKeyNames=c("nger", "Nakke", "norgast","nra","Muskel","noric101619","nir")
  #registrationFormNames=c("Operasjon","Legeskjema","Registrering","1A Anamnese","Basisregistrering","Start","nada")
  #peculiarity=c(rep(FALSE, 6), TRUE)
  for (i in (1:length(registryKeyNames))) {
    if (i == 1) {
      regDelays <- RegDelay(years = years, registryName = registryKeyNames[i],
                            registrationFormName = registrationFormNames[i],
                            peculiarity = peculiarity[i])
    }
    else {
      regDelays <- rbind(regDelays,
                         RegDelay(years = years,
                                  registryName = registryKeyNames[i],
                                  registrationFormName = 
                                    registrationFormNames[i],
                                  peculiarity = peculiarity[i]))
    }
  }
  
  # order data frame by sum days ascending
  regDelays <- regDelays[order(regDelays$sumDays), ]
  
  return(regDelays)
}