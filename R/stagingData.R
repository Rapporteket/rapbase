#' Staging data functions
#'
#' These functions are used to handle staging data at Rapporteket.
#'
#' @param registryName Character string providing the registry name
#' @param dataName Character string providing the data set name.
#' @param data A data object such as a data.frame to be stored as
#' \code{dataName}.
#' @param dir Character string providing the path to where the staging data
#' directory resides. Default value is \code{Sys.getenv("R_RAP_CONFIG_PATH")}.
#'
#' @return \code{pathStagingData()} returns a character string with the path to
#' the staging directory of \code{registryName}. If its parent directory
#' (\code{dir}) does not exists an error is returned. \code{saveStagingData()}
#' returns the data object (\code{data}), invisibly. \code{loadStagingData()}
#' returns the data object corresponding to the name given upon saving
#' (\code{dataName}). If the requested data set does not exist FALSE is
#' returned.
#' @name stagingData
#' @aliases pathStagingData saveStagingData loadStagingData
#'
#' @examples
#' ## Check and make the path to staging data
#' path <- pathStagingData(registryName = "myReg", dir = tempdir())
#'
#' ## Prep test data
#' registryName <- "myReg"
#' dataName <- "testData"
#' data <- mtcars
#' dir <- tempdir()
#'
#' ## Save data for staging
#' sageStagingData(registryName, dataName, data, dir)
#'
#' ## Retrieve data set from staging
#' loadStagingData(registryName, dataName, dir)
NULL


#' @rdname stagingData
#' @export
pathStagingData <- function(registryName, dir) {

  stopifnot(dir.exists(dir))

  parentPath <- "stagingData"

  path <- file.path(dir, parentPath, registryName)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  path
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
