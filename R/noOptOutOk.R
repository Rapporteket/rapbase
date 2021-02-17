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
  msg <- c(
    "Den er grei", "Om du sier det, s\u00e5", "Ok, da", "Javel, da",
    "Ja, da er det vel s\u00e5nn", "Sk\u00e5l for den!", "Fint", "Flott",
    "Klart", "Mottatt", "Akkurat", "Livet er herlig"
  )

  sample(msg, 1)
}
