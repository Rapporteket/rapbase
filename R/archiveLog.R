#' Archive functions for logs at Rapporteket
#'
#' Functions to manage archiving of logs at Rapporteket. To be applied mainly
#' as helpers within the raplog package.
#'
#' \code{crateArchive} simply creates a directory for the archived files to
#' live in.
#'
#' \code{logsOverSize} provides a character vector of relevant files
#' for archiving based on the given size limit and file name pattern.
#'
#' \code{archiveLog} do the actual archiving and deletes the source log files
#' after the archive files where sucessfully created. Returns prematurely NULL
#' if no log file(s) provided
#'
#' \code{cleanArchive} deletes files from the archive as their days are
#' numbered.
#'
#' @param archivePath String providing the path to the archive directory
#' @param logPath String providing the path to the log directory
#' @param logs String vector defining the log file names. Defaults to
#' \code{c("appLog.csv", "reportLog.csv")}
#' @param eolDays Integer age in days definig archive file end-of-life. When
#' \code{eoldays < 0} no archive files will be deleted. Default value is -1
#' @param overSize Integer size in bytes from where larger files will be listed
#' as candidates for archiving. Default value set to 1 Mb (1024 * 1000)
#' @param pattern String regexp defining file name pattern
#'
#' @name archive
#' @aliases createArchive logsOverSize archiveLog cleanArchive
#'
NULL


#' @rdname archive
#' @export
#' @examples
#' # Create an archive
#' createArchive(archivePath = tempfile())
createArchive <- function(archivePath) {
  if (dir.exists(archivePath)) {
    stop(paste0(
      "The directory '", archivePath, "' already exists. ",
      "You can't make me overwrite!"
    ))
  } else {
    dir.create(archivePath)
  }
}


#' @rdname archive
#' @export
#' @examples
#' # List all files (with alphanumeric names) larger than 1 Kb in tempdir()
#' logsOverSize(
#'   logPath = tempdir(), overSize = 1024,
#'   pattern = "^[1-9a-zA-Z]"
#' )
logsOverSize <- function(logPath, overSize = 1024 * 1000, pattern = ".csv$") {
  files <- file.info(list.files(logPath,
    pattern = pattern,
    full.names = TRUE
  ))

  if (dim(files)[1] < 1) {
    return(character())
  } else {
    # keep only oversized in list
    files <- rownames(files)[files[, "size"] > overSize]
    # remove dirs (if any) from list and return basename
    basename(files[!file.info(files)$isdir])
  }
}


#' @rdname archive
#' @export
#' @examples
#' # Archive a file under the same directory
#' fileName <- paste0(tempfile(), ".csv")
#' file.create(fileName)
#' write.csv(mtcars, fileName)
#' archiveLog(
#'   archivePath = dirname(fileName), logPath = dirname(fileName),
#'   logs = c(basename(fileName))
#' )
archiveLog <- function(archivePath, logPath,
                       logs = c("appLog.csv", "reportLog.csv")) {
  if (length(logs) < 1) {
    return(NULL)
  }

  if (!dir.exists(archivePath)) {
    stop(paste0(
      "Got '", archivePath, "' as target archive directory, ",
      "but it does not exist. Cannot ",
      "make anything sensible from that!"
    ))
  }
  if (!dir.exists(logPath)) {
    stop(paste0(
      "I'm told to archive logs from '", logPath, "', but it ",
      "does not exist. Out of options here!"
    ))
  }
  if (!all(file.exists(file.path(logPath, logs)))) {
    stop(paste0(
      "Some or all of the log files provided (",
      paste(logs, collapse = ", "), ") does not exist. That is ",
      "fishy and I refuse to continue archiving!"
    ))
  }

  in_file <- file.path(logPath, logs)
  # add time tag to archived files also
  out_file <- file.path(
    archivePath,
    paste0(
      format(Sys.time(), "%Y%m%d%H%M%S"),
      tools::file_path_sans_ext(logs),
      ".rda"
    )
  )
  mapply(rio::convert, in_file, out_file)

  # make sure archived files exists before deleting logs
  ok <- file.exists(out_file)
  if (all(ok)) {
    file.remove(in_file)
  } else {
    warning(paste0(
      "Something went wrong archiving the following file(s): ",
      out_file[!ok], ". Please check! No log files where deleted."
    ))
  }
}


#' @rdname archive
#' @export
#' @examples
#' # Do not delete any files
#' cleanArchive(archivePath = tempdir())
cleanArchive <- function(archivePath, eolDays = -1, pattern = ".rda$") {
  if (eolDays == -1) {
    return(NULL)
  } else {
    files <- file.info(list.files(archivePath,
      pattern = pattern,
      full.names = TRUE
    ))
    rmFiles <- rownames(files[difftime(Sys.time(), files[, "mtime"],
      units = "days"
    ) > eolDays, ])
    file.remove(rmFiles)
  }
}
