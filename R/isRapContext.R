#' isRapContext tell if you are in or outside a Rapporteket context
#' 
#' Call to this function will return TRUE when run on a system where the
#' environment varable \code{R_RAP_INSTANCE} is set to either "DEV", "TEST",
#' "QA" or "PRODUCTION" and FALSE otherwise
#'
#' @return Logical if system has a defined Rapporteket context
#' @export
#'
#' @examples
#' isRapContext()

isRapContext <- function() {
  
  if (Sys.getenv("R_RAP_INSTANCE") %in% c("DEV", "TEST", "QA", "PRODUCTION")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}