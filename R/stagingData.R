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
#'   \item \code{saveStagingData()} when successful returns the data object
#'     (\code{data}), invisibly. If saving fails a warning is issued and the
#'     function returns FALSE.
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
#' deleteStagingData cleanStagingData pathStagingData dbStagingData
#' dbStagingPrereq dbStagingConnection dbStagingProcess
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

  conf <- getConfig("rapbaseConfig.yml")$r$staging

  if (conf$target == "file") {
    path <- pathStagingData(registryName, dir)

    return(list.files(path))
  }

  if (conf$target == "db") {
    dbStagingPrereq(conf$key)
    query <- "SELECT name FROM data WHERE registry = ?;"
    params <- list(registryName)
    df <- dbStagingProcess(conf$key, query, params)

    return(df$name)
  }
}

#' @rdname stagingData
#' @export
mtimeStagingData <- function(registryName,
                             dir = Sys.getenv("R_RAP_CONFIG_PATH")) {

  conf <- getConfig("rapbaseConfig.yml")$r$staging

  if (conf$target == "file") {
    parentPath <- "stagingData"
    path <- file.path(dir, parentPath, registryName)
    f <- normalizePath(list.files(path, recursive = TRUE, full.names = TRUE))
    mtime <- file.mtime(f)

    names(mtime) <- basename(f)
  }

  if (conf$target == "db") {
    dbStagingPrereq(conf$key)
    query <- "SELECT mtime, name FROM data WHERE registry = ?;"
    params <- list(registryName)
    df <- dbStagingProcess(conf$key, query, params)
    mtime <- as.POSIXct(df$mtime)
    names(mtime) <- df$name
  }
  mtime
}

#' @rdname stagingData
#' @export
saveStagingData <- function(registryName, dataName, data,
                            dir = Sys.getenv("R_RAP_CONFIG_PATH")) {
  conf <- getConfig("rapbaseConfig.yml")$r$staging

  if (conf$target == "file") {
    path <- pathStagingData(registryName, dir)
    return(
      invisible(
        readr::write_rds(data, file.path(path, dataName))
      )
    )
  }

  if (conf$target == "db") {
    dbStagingPrereq(conf$key)
    b <- memCompress(
      serialize(data, connection = NULL),
      type = "bzip2"
    )

    # remove any existing registry data with same data name (should never fail)
    query <- "DELETE FROM data WHERE registry = ? AND name = ?;"
    params <- list(registryName, dataName)
    d <- dbStagingProcess(conf$key, query, params, statement = TRUE)

    # insert new data (can fail, but hard to test...)
    query <- "INSERT INTO data (registry, name, data) VALUES (?, ?, ?);"
    params <- list(registryName, dataName, blob::as_blob(b))
    d <- dbStagingProcess(conf$key, query, params, statement = TRUE)
    if (d > 0) {
      return(invisible(data))
    } else {
      warning(paste0("The data set '", dataName, "' could not be saved!"))
      return(FALSE)
    }
  }
}

#' @rdname stagingData
#' @export
loadStagingData <- function(registryName, dataName,
                            dir = Sys.getenv("R_RAP_CONFIG_PATH")) {

  conf <- getConfig("rapbaseConfig.yml")$r$staging

  if (conf$target == "file") {
    path <- pathStagingData(registryName, dir)
    filePath <- file.path(path, dataName)

    if (file.exists(filePath)) {
      data <- readr::read_rds(filePath)
    } else {
      data <- FALSE
    }
  }

  if (conf$target == "db") {
    dbStagingPrereq(conf$key)
    query <- "SELECT data FROM data WHERE registry = ? AND name = ?;"
    params <- list(registryName, dataName)
    df <- dbStagingProcess(conf$key, query, params)
    if (length(df$data) == 0) {
      data <- FALSE
    } else {
      data <- df$data[[1]] %>%
        memDecompress(type = "bzip2") %>%
        unserialize()
    }
  }

  data
}

#' @rdname stagingData
#' @export
deleteStagingData <- function(registryName, dataName,
                              dir = Sys.getenv("R_RAP_CONFIG_PATH")) {

  conf <- getConfig("rapbaseConfig.yml")$r$staging

  if (conf$target == "file") {
    path <- pathStagingData(registryName, dir)
    filePath <- file.path(path, dataName)

    if (file.exists(filePath)) {
      file.remove(filePath)
      isDelete <- TRUE
    } else {
      isDelete <- FALSE
    }
  }

  if (conf$target == "db") {
    dbStagingPrereq(conf$key)
    query <- "DELETE FROM data WHERE registry = ? AND name = ?;"
    params <- list(registryName, dataName)
    d <- dbStagingProcess(conf$key, query, params, statement = TRUE)
    if (d > 0) {
      isDelete <- TRUE
    } else {
      isDelete <- FALSE
    }
  }

  isDelete
}

#' @rdname stagingData
#' @export
cleanStagingData <- function(eolAge, dryRun = TRUE) {
  if (Sys.getenv("R_RAP_CONFIG_PATH") == "") {
    stop(paste(
      "No data store provided. Hence, no data will be deleted.",
      "Exiting."
    ))
  }

  conf <- getConfig("rapbaseConfig.yml")$r$staging

  if (conf$target == "file") {
    dir <- Sys.getenv("R_RAP_CONFIG_PATH")
    parentPath <- "stagingData"
    path <- file.path(dir, parentPath)
    f <- normalizePath(list.files(path, recursive = TRUE, full.names = TRUE))
    fAge <- as.numeric(Sys.time()) - as.numeric(file.mtime(f))
    deleteDataset <- f[fAge > eolAge]
  }

  if (conf$target == "db") {
    dbStagingPrereq(conf$key)
    eolTime <- Sys.time() - eolAge
    query <- paste0(
      "SELECT registry, name FROM data WHERE mtime < ? ORDER BY registry, name;"
    )
    params <- list(eolTime)
    df <- dbStagingProcess(conf$key, query, params)
    deleteDataset <- paste0(df$registry, ": ", df$name)
  }

  if (dryRun) {
    message(
      paste(
        "Function invoked in dry run mode and none of the returned staging\n",
        "data sets will be deleted.\n",
        "To delete for real, please contemplate and re-run this function\n",
        "with the dryRun argument set to 'FALSE'. Godspeed!"
      )
    )
    deleteDataset
  } else {
    if (conf$target == "file") {
      file.remove(deleteDataset)
    }
    if (conf$target == "db") {
      query <- "DELETE FROM data WHERE mtime < ?;"
      d <- dbStagingProcess(conf$key, query, params, statement = TRUE)
    }
    invisible(deleteDataset)
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

#' @rdname stagingData
dbStagingData <- function(key, drop = FALSE) {

  conf <- getConfig()[[key]]
  if (is.null(conf)) {
    stop(paste("There is no configuration corresponding to key", key))
  }
  if (drop) {
    query <- paste("DROP DATABASE", conf$name)
    msg <- paste0("Database '", conf$name, "' deleted.")
  } else {
    query <- c(
      sprintf(
        readLines(system.file("createStagingDb.sql", package = "rapbase")),
        conf$name
      ),
      paste0(
        readLines(system.file("createStagingTab.sql", package = "rapbase")),
        collapse = "\n"
      )
    )
    msg <- paste0("Database '", conf$name, "exists.")
  }

  con <- dbStagingConnection(key = key, init = TRUE)
  for (q in query) {
    tmp <- RMariaDB::dbExecute(con, q)
  }

  con <- dbStagingConnection(con = con)

  invisible(msg)
}

#' @rdname stagingData
dbStagingPrereq <- function(key) {

  con <- dbStagingConnection(key, init = TRUE)
  query <- "SHOW DATABASES LIKE 'staging';"
  df <- RMariaDB::dbGetQuery(con, query)
  # close and remove db connection
  con <- dbStagingConnection(con = con)
  if (length(df$Database) > 0) {
    msg <- "You're good! Database for staging data already exists."
  } else {
    dbStagingData(key)
    msg <- "Database for staging data was created."
  }

  invisible(msg)
}

#' @rdname stagingData
dbStagingConnection <- function(key = NULL, con = NULL, init = FALSE) {

  if (inherits(con, "DBIConnection")) {
    con <- DBI::dbDisconnect(con)
    con <- NULL
    return(invisible(con))
  }

  if (!is.null(key)) {
    conf <- getConfig()[[key]]
    if (is.null(conf)) {
      stop(
        paste0(
          "Could not connect to database because there is no configuration ",
          "corresponding to key '", key,"'. Please check key and/or ",
          "configuration."
        )
      )
    }
    if (init) {
      dbname <- NULL
    } else {
      dbname <- conf$name
    }
    drv <- RMariaDB::MariaDB()
    con <- RMariaDB::dbConnect(
      drv,
      dbname,
      host = conf$host,
      user = conf$user,
      password = conf$pass
    )
    return(con)
  } else {
    stop("Either a key or a valid database connection object must be provided.")
  }
}

#' @rdname stagingData
dbStagingProcess <- function(key, query, params = list(), statement = FALSE) {

  con <- dbStagingConnection(key)
  if (statement) {
    df <- RMariaDB::dbExecute(con, query, params)
  } else {
    rs <- RMariaDB::dbSendQuery(con, query)
    RMariaDB::dbBind(rs, params)
    df <- RMariaDB::dbFetch(rs)
    RMariaDB::dbClearResult(rs)
  }
  con <- dbStagingConnection(con = con)

  df
}