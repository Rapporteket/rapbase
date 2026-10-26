#' Provide connection handle for data source at Rapporteket
#'
#' Generic to registries, handle the data source connections, including
#' usernames and passwords needed to open these connections
#'
#' @param dbName String providing the name of the database to connect to. If it
#' is "data" it will use the MYSQL_DB_DATA environment variable, if it is
#' "autoreport" it will use the MYSQL_DB_AUTOREPORT environment variable, and
#' if it is "raplog" it will use the MYSQL_DB_LOG environment variable. If
#' none of these are set, it will use the name provided.
#' @param dbType String providing type of data source, one of
#' "mysql", "sqlite" and "mssql". Defaults to "mysql". If the environment
#' variable DB_TYPE is set, it will override this argument.
#' @return A named list of con and drv representing the db connection handle and
#'   driver, respectively.
#' @export

rapOpenDbConnection <- function(dbName, dbType = "mysql") {
  if (Sys.getenv("DB_TYPE") %in% c("mysql", "sqlite", "mssql")) {
    message(
      "Overriding dbType to ",
      Sys.getenv("DB_TYPE"),
      " from environment variable DB_TYPE."
    )
    dbType <- Sys.getenv("DB_TYPE")
  }

  if (dbType == "mysql") {
    if (!all(
      c("MYSQL_HOST", "MYSQL_USER", "MYSQL_PASSWORD") %in% names(Sys.getenv())
    )) {
      stop(paste0(
        "Could not connect to database because the enviroment ",
        "variables MYSQL_HOST, MYSQL_USER and/or MYSQL_PASSWORD ",
        "are not defined. Please check configuration."
      ))
    }
    db <- getDbName(dbName)
    drv <- RMariaDB::MariaDB()
    con <- DBI::dbConnect(
      drv,
      dbname = db,
      host = Sys.getenv("MYSQL_HOST"),
      user = Sys.getenv("MYSQL_USER"),
      password = Sys.getenv("MYSQL_PASSWORD"),
      bigint = "integer",
      port = as.numeric(Sys.getenv("MYSQL_PORT", "3306"))
    )
    message("Connected to database: ", db)
    # ensure utf8 encoding
    invisible(DBI::dbExecute(con, "SET NAMES utf8;"))
  } else if (dbType == "mssql") {
    if (!("MYSQL_HOST" %in% names(Sys.getenv()))) {
      stop(paste(
        "Environment variable MYSQL_HOST is not defined.",
        "Please check configuration."
      ))
    }
    rlang::check_installed("odbc")
    drv <- odbc::odbc()
    db <- getDbName(dbName)
    con <- DBI::dbConnect(
      drv,
      driver = Sys.getenv("MYSQL_DRIVER", "FreeTDS"),
      database = db,
      server = Sys.getenv("MYSQL_HOST"),
      UID = Sys.getenv("MYSQL_USER", ""),
      PWD = Sys.getenv("MYSQL_PASSWORD", ""),
      Port = as.numeric(Sys.getenv("MYSQL_PORT", "1433")),
      TrustedConnection = "Yes",
      Timeout = 10
    )
    message("Connected to mssql-database: ", db)
  } else if (dbType == "sqlite") {
    db <- getDbName(dbName)
    rlang::check_installed("RSQLite")
    drv <- RSQLite::SQLite()
    con <- DBI::dbConnect(
      drv,
      dbname = db
    )
    message("Connected to sqlite-database: ", db)
  } else {
    stop(paste0(
      "Unsupported dbType ", dbType,
      ". Use 'mysql', 'sqlite' or 'mssql'."
    ))
  }

  list(con = con, drv = drv)
}


#' Close down data connection handle
#'
#' @param con Open connection object that is to be closed
#' @export

rapCloseDbConnection <- function(con) {
  con <- DBI::dbDisconnect(con)
  con <- NULL
}

#' Get database name
#'
#' @param dbName String providing the name of the database to connect to. If it
#' is "data" it will use the MYSQL_DB_DATA environment variable, if it is
#' "autoreport" it will use the MYSQL_DB_AUTOREPORT environment variable, and
#' if it is "raplog" it will use the MYSQL_DB_LOG environment variable. If
#' none of these are set, it will use the name provided.
#'
#' @return String providing the name of the database.
#'
#' @keywords internal
#'
getDbName <- function(dbName = "data") {
  name <- switch(
    dbName,
    "raplog" = Sys.getenv("MYSQL_DB_LOG"),
    "autoreport" = Sys.getenv("MYSQL_DB_AUTOREPORT"),
    "staging" = Sys.getenv("MYSQL_DB_STAGING"),
    "data" = Sys.getenv("MYSQL_DB_DATA"),
    dbName
  )
  return(name)
}
