#' Staging data functions
#'
#' Low level functions for handling registry staging data at Rapporteket. As
#' such, these functions does not provide staging data management \emph{per se}.
#' Proper management, \emph{e.g.} staging data updates and fallback logic
#' must therefore be established within each registry that take staging data
#' into use.
#'
#' Staging data can be stored as files or as binary large objects in a database
#' and method of choice is defined by the \code{rapbase} configuration.
#' Regardless of storage method a per registry symmetric encryption of storage
#' content is enforced. Keys used for encryption are generated from existing
#' database credentials. Therefore, please note that removing or changing
#' such credentials will render any historic staging data inaccessible.
#'
#' \code{cleanStagingData()} globally removes all staging data with store date
#' prior to the end-of-life age provided. This is a vastly destructive function
#' that should be used with great care.
#'
#' @param registryName Character string providing the registry name.
#' @param dataName Character string providing the data set name.
#' @param data A data object such as a data.frame to be stored as
#' \code{dataName}.
#' @param dir Character string providing the path to where the staging data
#'   directory resides in case of storage as files. Default value is
#'   \code{Sys.getenv("R_RAP_CONFIG_PATH")}.
#' @param eolAge Numeric providing the staging data end-of-life age in seconds.
#'   Based on the current time and the time of storage staging files
#'   older than \code{eolAge} will be identified as subject for removal.
#' @param dryRun Logical defining if function is to be run in dry (none
#'   destructive) mode.
#'
#' @return \itemize{
#'   \item \code{listStagingData()} returns a character vector of staging data
#'     sets for the given registry (\code{registryName}).
#'   \item \code{mtimeStagingData()} returns a staging data set named POSIXct
#'     vector of modification times for the given registry
#'     (\code{registryName}).
#'   \item \code{saveStagingData()} when successful returns the data object
#'     (\code{data}), invisibly. If saving fails a warning is issued and the
#'     function returns FALSE.
#'   \item \code{loadStagingData()} returns the data object corresponding to
#'     the name given upon saving (\code{dataName}). If the requested data set
#'     for loading does not exist the function returns FALSE.
#'   \item \code{deleteStagingData()} returns TRUE if the data set was deleted
#'     and FALSE if not.
#'   \item \code{cleanStagingData()} returns a list of data sets (to be)
#'     removed.
#'   \item \code{rapbase:::pathStagingData()} is an internal helper function and
#'     returns a character string with the path to the staging directory of
#'     \code{registryName}. If its parent directory (\code{dir}) does not exists
#'     an error is returned.
#' }
#'
#' @name stagingData
#' @aliases listStagingData mtimeStagingData saveStagingData loadStagingData
#' deleteStagingData cleanStagingData
#'
#' @examples
#' ## Prep test data
#' registryName <- "rapbase"
#' dataName <- "testData"
#' data <- mtcars
#' dir <- tempdir()
#'
#' ## Save data for staging
#' saveStagingData(registryName, dataName, data, dir)
#'
#' ## List data currently in staging
#' listStagingData(registryName, dir)
#'
#' ## Retrieve data set from staging and compare to outset
#' stagedData <- loadStagingData(registryName, dataName, dir)
#' identical(data, stagedData)
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
  b <- wrapStagingData(data, registryName) %>%
    blob::as_blob()

  if (conf$target == "file") {
    path <- pathStagingData(registryName, dir)
    saveRDS(b, file.path(path, dataName))
  }

  if (conf$target == "db") {
    dbStagingPrereq(conf$key)

    # remove any existing registry data with same data name (should never fail)
    query <- "DELETE FROM data WHERE registry = ? AND name = ?;"
    params <- list(registryName, dataName)
    d <- dbStagingProcess(conf$key, query, params, statement = TRUE)

    # insert new data (can fail, but hard to test...)
    query <- "INSERT INTO data (registry, name, data) VALUES (?, ?, ?);"
    params <- list(registryName, dataName, b)
    d <- dbStagingProcess(conf$key, query, params, statement = TRUE)
    if (d < 1) {
      warning(paste0("The data set '", dataName, "' could not be saved!"))
      data <- FALSE
    }
  }

  invisible(data)
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
      b <- readRDS(filePath)
      # raw is first element in blob list
      data <- unwrapStagingData(b[[1]], registryName)
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
      # raw is first element in blob list
      data <- unwrapStagingData(df$data[[1]], registryName)
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
      dbStagingProcess(conf$key, query, params, statement = TRUE)
    }
    invisible(deleteDataset)
  }
}


#' Data staging helper (internal) functions
#'
#' A set of helper functions to aid staging of registry data at Rapporteket.
#'
#'
#' @param registryName Character string providing the registry name.
#' @param dir Character string providing the path to where the staging data
#'   directory resides in case of storage as files. Default value is
#'   \code{Sys.getenv("R_RAP_CONFIG_PATH")}.
#' @param data A data object that is to be added to or collected from staging.
#' @param key Character string with key to be used for staging data store
#'   credentials.
#' @param drop Logical defining if a database is to be deleted. FALSE by
#'   default.
#' @param con A database connection object.
#' @param init Logical defining if the function call will perform an initial
#'   set-up of a database. Default value is FALSE
#' @param query Character string providing a database query.
#' @param params List of values to be provided in a parameterized query.
#' @param statement Logical defining if a query is a statement or not. Default
#'   value is FALSE.
#'
#' @return \itemize{
#'   \item \code{pathStagingData()} returns a character string with the path to
#'     the staging directory of \code{registryName}. If its parent directory
#'     (\code{dir}) does not exists an error is returned.
#'   \item \code{dbStagingData()} creates or drops a staging data database and
#'     returns a message invisibly.
#'   \item \code{dbStagingPrereq()} ensures that a database for staging data is
#'     properly setup and returns a message, invisibly.
#'   \item \code{dbStagingConnection()} returns an open database connection
#'     object or, when an open connection object is provided as an argument,
#'     closes it and returns \code{NULL} invisibly.
#'   \item \code{dbStagingProcess()} returns the raw result of a database query
#'     based on the arguments provided.
#' }
#'
#' @name stagingDataHelper
#' @keywords internal
#' @aliases pathStagingData dbStagingData dbStagingPrereq dbStagingConnection
#'   dbStagingProcess
NULL

#' @rdname stagingDataHelper
pathStagingData <- function(registryName, dir) {
  stopifnot(dir.exists(dir))

  parentPath <- "stagingData"

  path <- file.path(dir, parentPath, registryName)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  path
}

#' @rdname stagingDataHelper
wrapStagingData <- function(data, key) {

  k <- digest::digest(getConfig()[[key]]$pass, algo = "sha256", raw = TRUE)
  serialize(data, connection = NULL) %>%
    sship::sym_enc(key = k, iv = NULL) %>%
    memCompress(type = "bzip2")
}

#' @rdname stagingDataHelper
unwrapStagingData <- function(data, key) {

  k <- digest::digest(getConfig()[[key]]$pass, algo = "sha256", raw = TRUE)
  memDecompress(data, type = "bzip2") %>%
    sship::sym_dec(key = k, iv = NULL) %>%
    unserialize()
}

#' @rdname stagingDataHelper
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
    invisible(RMariaDB::dbExecute(con, q))
  }

  con <- dbStagingConnection(con = con)

  invisible(msg)
}

#' @rdname stagingDataHelper
dbStagingPrereq <- function(key) {

  conf <- getConfig()[[key]]
  if (is.null(conf)) {
    stop(paste("There is no configuration corresponding to key", key))
  }

  con <- dbStagingConnection(key, init = TRUE)
  query <- paste0("SHOW DATABASES LIKE '", conf$name, "';")
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

#' @rdname stagingDataHelper
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
          "corresponding to key '", key, "'. Please check key and/or ",
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

#' @rdname stagingDataHelper
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

  invisible(df)
}
