#' Management of shiny application and report logging at Rapporteket
#'
#' A tool to aid archiving and cleanup of logs containing data on use of shiny
#' application and reports at Rapporteket. This function is ment to be called
#' from a scheduler (\emph{e.g.} chron) directly or as part of a process chain
#' initiated by a scheduler
#'
#' @seealso To become orchestrator
#' \code{\link[rapbase:runAutoReport]{runAutoReport}} in package \emph{rapbase}
#'
#' @param overSize Integer size in bytes from where larger files will be listed
#' as candidates for archiving. Not used by this function \emph{per se} but
#' rather passed on to \code{logsOverSize()}. Normally, its value does not need
#' to be set but may be convenient in rare cases (such as full scale tests).
#' Default value set to 1 Mb (1024 * 1000)
#'
#' @return Silently exits after successful file operations
#' @export
#'
#' @examples
#' \donttest{
#' # Depend on the R_RAP_CONFIG_PATH being defined and that relevant
#' # configuration is present
#' try(raplogManager())
#' }
#'
raplogManager <- function(overSize = 1024 * 1000) {
  logPath <- Sys.getenv("R_RAP_CONFIG_PATH")
  if (logPath == "") {
    stop("No path to configuration data provided. Cannot continue!")
  }

  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")

  archiveDir <- conf$r$raplog$archiveDir
  archivePath <- file.path(logPath, archiveDir)

  if (!dir.exists(archivePath)) {
    createArchive(archivePath)
  }

  ripeLogs <- logsOverSize(logPath, overSize)
  archiveLog(archivePath, logPath, logs = ripeLogs)

  eolDays <- conf$r$raplog$eolDays
  cleanArchive(archivePath, eolDays)
  invisible()
}
