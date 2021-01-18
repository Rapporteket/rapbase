#' Kick off functions at Rapporteket
#'
#' This function will normally be executed by a cron daemon. Once started this
#' function will nest through schedule functions defined in a configuration
#' file, \emph{e.g.} "rapbaseConfig.yml".
#'
#' This is a crontab example running fireInTheHole() every night at 01 hours,
#' Monday throug Friday and with emails supressed:
#' \preformatted{0  1 * * 1-5 Rscript -e 'rapbase::fireInTheHole()' >/dev/null
#' 2>&1}
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
fireInTheHole <- function() {
  conf <- getConfig(fileName = "rapbaseConfig.yml")
  funs <- conf$r$schedule$nocturnal$funs

  for (f in funs) {
    do.call(what = .getFun(f), args = list())
  }
}
