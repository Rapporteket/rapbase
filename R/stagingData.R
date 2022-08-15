#' Staging data functions
#'
#' Low level functions for handling registry staging data at Rapporteket. As
#' such, these functions does not provide staging data management \emph{per se}.
#' Proper management, \emph{e.g.} staging data updates and fallback logic
#' must therefore be established within each registry that take staging data
#' into use.
#'
#' \code{cleanStagingData()} globally removes all staging data files older than
#' the end-of-life age provided. This is potentially a vastly destructive
#' function that should be used with great care.
#'
#' @param registryName Character string providing the registry name.
#' @param dataName Character string providing the data set name.
#' @param data A data object such as a data.frame to be stored as
#' \code{dataName}.
#' @param dir Character string providing the path to where the staging data
#' directory resides. Default value is \code{Sys.getenv("R_RAP_CONFIG_PATH")}.
#' @param eolAge Numeric providing the staging file end-of-life age in seconds.
#' Based on the current time and the file modification time stamp staging files
#' older than \code{eolAge} will be identified as subject for removal.
#' @param dryRun Logical defining if function is to be run in dry (none
#' destructive) mode.
#'
#' @return \itemize{
#'   \item \code{listStagingData()} returns a character vector of staging data
#'     files for the given registry (\code{registryName}).
#'   \item \code{mtimeStagingData()} returns a staging file-named POSIXct vector
#'     of modification times for the given registry (\code{registryName}).
#'   \item \code{saveStagingData()} returns the data object (\code{data}),
#'     invisibly.
#'   \item \code{loadStagingData()} returns the data object corresponding to
#'     the name given upon saving (\code{dataName}). If the requested data set
#'     for loading does not exist the function returns FALSE.
#'   \item \code{deleteStagingData()} returns TRUE if the file was deleted and
#'     FALSE if not.
#'   \item \code{cleanStagingData()} returns a list of files (to be) removed.
#'   \item \code{rapbase:::pathStagingData()} is an internal helper function and
#'     returns a character string with the path to the staging directory of
#'     \code{registryName}. If its parent directory (\code{dir}) does not exists
#'     an error is returned.
#' }
#'
#' @name stagingData
#' @aliases listStagingData mtimeStagingData saveStagingData loadStagingData
#' deleteStagingData cleanStagingData pathStagingData
#'
#' @examples
#' ## Prep test data
#' registryName <- "myReg"
#' dataName <- "testData"
#' data <- mtcars
#' dir <- tempdir()
#'
#' ## Save data for staging
#' saveStagingData(registryName, dataName, data, dir)
#'
#' ## List files currently in staging
#' listStagingData(registryName, dir)
#'
#' ## Retrieve data set from staging
#' loadStagingData(registryName, dataName, dir)
#'
#' ## Get modification time for staging file(s)
#' mtimeStagingData(registryName, dir)
NULL

#' @rdname stagingData
#' @export
listStagingData <- function(registryName,
                            dir = Sys.getenv("R_RAP_CONFIG_PATH")) {
  path <- pathStagingData(registryName, dir)

  list.files(path)
}

#' @rdname stagingData
#' @export
mtimeStagingData <- function(registryName,
                             dir = Sys.getenv("R_RAP_CONFIG_PATH")) {
  parentPath <- "stagingData"
  path <- file.path(dir, parentPath, registryName)
  f <- normalizePath(list.files(path, recursive = TRUE, full.names = TRUE))
  mtime <- file.mtime(f)

  names(mtime) <- basename(f)

  mtime
}

#' @rdname stagingData
#' @export
saveStagingData <- function(registryName, dataName, data,
                            dir = Sys.getenv("R_RAP_CONFIG_PATH")) {
  path <- pathStagingData(registryName, dir)

  readr::write_rds(data, file.path(path, dataName))
}

#' @rdname stagingData
#' @export
loadStagingData <- function(registryName, dataName,
                            dir = Sys.getenv("R_RAP_CONFIG_PATH")) {
  path <- pathStagingData(registryName, dir)
  filePath <- file.path(path, dataName)

  if (file.exists(filePath)) {
    readr::read_rds(filePath)
  } else {
    FALSE
  }
}

#' @rdname stagingData
#' @export
deleteStagingData <- function(registryName, dataName,
                              dir = Sys.getenv("R_RAP_CONFIG_PATH")) {
  path <- pathStagingData(registryName, dir)
  filePath <- file.path(path, dataName)

  if (file.exists(filePath)) {
    file.remove(filePath)
    TRUE
  } else {
    FALSE
  }
}

#' @rdname stagingData
#' @export
cleanStagingData <- function(eolAge, dryRun = TRUE) {
  if (Sys.getenv("R_RAP_CONFIG_PATH") == "") {
    stop(paste(
      "Got no path to staging data. No data will be deleted.",
      "Exiting."
    ))
  }

  dir <- Sys.getenv("R_RAP_CONFIG_PATH")
  parentPath <- "stagingData"
  path <- file.path(dir, parentPath)
  f <- normalizePath(list.files(path, recursive = TRUE, full.names = TRUE))
  fAge <- as.numeric(Sys.time()) - as.numeric(file.mtime(f))
  fDelete <- f[fAge > eolAge]

  if (dryRun) {
    message(
      paste(
        "Function invoked in dry run mode and none of the returned files\n",
        "will be deleted.\n",
        "To delete the files please re-run this function with the dryRun\n",
        "argument set to 'TRUE'. Godspeed!"
      )
    )
    fDelete
  } else {
    file.remove(fDelete)
    invisible(fDelete)
  }
}

#' @rdname stagingData
pathStagingData <- function(registryName, dir) {
  stopifnot(dir.exists(dir))

  parentPath <- "stagingData"

  path <- file.path(dir, parentPath, registryName)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  path
}
