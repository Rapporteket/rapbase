#' Provide a no-opt-out ok message
#' 
#' To be applied for user input when there is actaully no choice :-)
#'
#' @return String with possible state of mind (in Norwegian) once left with no
#' opitions 
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