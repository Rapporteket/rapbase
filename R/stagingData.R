#' Staging data functions
#'
#' Low level functions for handling registry staging data at Rapporteket. As
#' such, these functions does not provide staging data management \emph{per se}.
#' Proper management, \emph{e.g.} staging data updates and fallback logic
#' must therefore be established within each registry that take staging data
#' into use.
#'
#' Staging data is stored as binary large objects in a database.
#' A per registry symmetric encryption of storage
#' content is enforced. Keys used for encryption are generated from existing
#' database credentials. Therefore, please note that removing or changing
#' such credentials will render any historic staging data inaccessible.
#'
#' \code{cleanStagingData()} globally removes all staging data with store date
#' prior to the end-of-life age provided. This is a vastly destructive function
#' that should be used with great care.
#'
#' @param registryName Character string providing the registry name.
#' @param dbTable Character string providing the database table name to be used
#' @param dataName Character string providing the data set name.
#' @param data A data object such as a data.frame to be stored as
#' \code{dataName}.
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
#' }
#'
#' @name stagingData
#' @aliases listStagingData mtimeStagingData saveStagingData loadStagingData
#' deleteStagingData cleanStagingData
#'
#' @examples
#' \donttest{
#' ## Prep test data
#' registryName <- "rapbase"
#' dataName <- "testData"
#' data <- mtcars
#'
#' ## Save data for staging
#' try(saveStagingData(registryName, dataName, data))
#'
#' ## List data currently in staging
#' try(listStagingData(registryName))
#'
#' ## Retrieve data set from staging and compare to outset
#' try(stagedData <- loadStagingData(registryName, dataName))
#' try(identical(data, stagedData))
#'
#' ## Get modification time for staging file(s)
#' try(mtimeStagingData(registryName))
#' }
NULL

#' @rdname stagingData
#' @export
listStagingData <- function(registryName, dbTable = "data") {

  dbStagingPrereq("staging")
  query <- "SELECT name FROM ? WHERE registry = ?;"
  params <- list(dbTable, registryName)
  df <- dbStagingProcess("staging", query, params)
  df$name
}

#' @rdname stagingData
#' @export
mtimeStagingData <- function(registryName, dbTable = "data") {

  dbStagingPrereq("staging")
  query <- "SELECT mtime, name FROM ? WHERE registry = ?;"
  params <- list(dbTable, registryName)
  df <- dbStagingProcess("staging", query, params)
  mtime <- as.POSIXct(df$mtime)
  names(mtime) <- df$name
  mtime
}

#' @rdname stagingData
#' @export
saveStagingData <- function(registryName, dataName, data, dbTable = "data") {
  b <- wrapStagingData(data) %>%
    blob::as_blob()

  dbStagingPrereq("staging")

  # remove any existing registry data with same data name (should never fail)
  query <- "DELETE FROM ? WHERE registry = ? AND name = ?;"
  params <- list(dbTable, registryName, dataName)
  d <- dbStagingProcess("staging", query, params, statement = TRUE)

  # insert new data (can fail, but hard to test...)
  query <- "INSERT INTO ? (registry, name, data) VALUES (?, ?, ?);"
  params <- list(dbTable, registryName, dataName, b)
  d <- dbStagingProcess("staging", query, params, statement = TRUE)
  if (d < 1) {
    warning(paste0("The data set '", dataName, "' could not be saved!"))
    data <- FALSE
  }

  invisible(data)
}

#' @rdname stagingData
#' @export
loadStagingData <- function(registryName, dataName, dbTable = "data") {

  dbStagingPrereq("staging")
  query <- "SELECT data FROM ? WHERE registry = ? AND name = ?;"
  params <- list(dbTable, registryName, dataName)
  df <- dbStagingProcess("staging", query, params)
  if (length(df$data) == 0) {
    data <- FALSE
  } else {
    # raw is first element in blob list
    data <- unwrapStagingData(df$data[[1]])
  }

  data
}

#' @rdname stagingData
#' @export
deleteStagingData <- function(registryName, dataName, dbTable = "data") {

  dbStagingPrereq("staging")
  query <- "DELETE FROM ? WHERE registry = ? AND name = ?;"
  params <- list(dbTable, registryName, dataName)
  d <- dbStagingProcess("staging", query, params, statement = TRUE)
  if (d > 0) {
    isDelete <- TRUE
  } else {
    isDelete <- FALSE
  }

  isDelete
}

#' @rdname stagingData
#' @export
cleanStagingData <- function(eolAge, dryRun = TRUE, dbTable = "data") {

  dbStagingPrereq("staging")
  eolTime <- Sys.time() - eolAge
  query <- paste0(
    "SELECT registry, name FROM ? WHERE mtime < ? ORDER BY registry, name;"
  )
  params <- list(dbTable, eolTime)
  df <- dbStagingProcess("staging", query, params)
  deleteDataset <- paste0(df$registry, ": ", df$name)

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
    query <- "DELETE FROM ? WHERE mtime < ?;"
    dbStagingProcess("staging", query, params, statement = TRUE)
    invisible(deleteDataset)
  }
}


#' Data staging helper (internal) functions
#'
#' A set of helper functions to aid staging of registry data at Rapporteket.
#'
#'
#' @param data A data object that is to be added to or collected from staging.
#' @param key Character string with key to be used for staging data store
#'   credentials.
#' @param con A database connection object.
#' @param init Logical defining if the function call will perform an initial
#'   set-up of a database. Default value is FALSE
#' @param query Character string providing a database query.
#' @param params List of values to be provided in a parameterized query.
#' @param statement Logical defining if a query is a statement or not. Default
#'   value is FALSE.
#'
#' @return \itemize{
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
#' @aliases dbStagingPrereq dbStagingConnection
#'   dbStagingProcess
NULL

#' @rdname stagingDataHelper
wrapStagingData <- function(data) {

  k <- digest::digest(Sys.getenv("MYSQL_PASSWORD"), algo = "sha256", raw = TRUE)
  serialize(data, connection = NULL) %>%
    sship::sym_enc(key = k, iv = NULL) %>%
    memCompress(type = "bzip2")
}

#' @rdname stagingDataHelper
unwrapStagingData <- function(data) {

  k <- digest::digest(Sys.getenv("MYSQL_PASSWORD"), algo = "sha256", raw = TRUE)
  memDecompress(data, type = "bzip2") %>%
    sship::sym_dec(key = k, iv = NULL) %>%
    unserialize()
}

#' @rdname stagingDataHelper
dbStagingPrereq <- function(key) {

  conf <- getDbConfig(key)

  con <- dbStagingConnection(key, init = TRUE)
  query <- paste0("SHOW DATABASES LIKE '", conf$name, "';")
  df <- RMariaDB::dbGetQuery(con, query)
  # close and remove db connection
  con <- dbStagingConnection(con = con)
  if (length(df$Database) > 0) {
    msg <- "You're good! Database for staging data already exists."
  } else {
    stop(paste0(
      "Database for staging (", conf$name, ") does not exist."
    ))
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
    con <- rapOpenDbConnection(dbName = key)$con
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
