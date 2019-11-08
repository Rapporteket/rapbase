#' Provide a no-opt-out ok message
#' 
#' To be applied for user input when there is actuaully no choice :-)
#'
#' @return String with possible state of mind (in Norwegian) once left with no
#' options 
#' @export
#'
#' @examples
#' noOptOutOk()

noOptOutOk <- function() {
  
  msg <- c("Den er grei", "Om du sier det, så", "Ok, da", "Javel, da",
           "Ja, da er det vel sånn", "Skål for den!", "Fint", "Flott",
           "Klart", "Mottatt", "Akkurat", "Livet er herlig")
  
  sample(msg, 1)
}