#' Kick off functions at Rapporteket
#'
#' This function will normally be executed by a cron daemon. Once started this
#' function will nest through schedule functions defined in a configuration
#' file, \emph{e.g.} "rapbaseConfig.yml".
#'
#' This is a crontab example running fireInTheHole() every night at 01 hours,
#' Monday through Friday and with emails suppressed:
#' \preformatted{0  1 * * 1-5 Rscript -e 'rapbase::fireInTheHole()' >/dev/null
#' 2>&1}
#'
#' @param flipPeriod Logical only used for testing. FALSE by default
#'
#' @return NULL
#' @export
#'
#' @examples
#' \donttest{
#' # Depends on the env var R_RAP_CONFIG_PATH being properly set
#' fireInTheHole()
#' }
#'
fireInTheHole <- function(flipPeriod = FALSE) {
  hour <- as.POSIXlt(Sys.time())$hour
  conf <- getConfig(fileName = "rapbaseConfig.yml")

  if (hour >= conf$r$schedule$nocturnal$startHour &&
    hour < conf$r$schedule$nocturnal$endHour) {
    night <- TRUE
  } else {
    night <- FALSE
  }

  if (flipPeriod) {
    night <- !night
  }

  if (night) {
    funs <- conf$r$schedule$nocturnal$funs
  } else {
    funs <- conf$r$schedule$diurnal$funs
  }

  for (f in funs) {
    do.call(what = .getFun(f), args = list())
  }
}
