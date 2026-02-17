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
#'   "mysql" and "sqlite". Defaults to "mysql". "mssql" is not supported
#' anymore.
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
    conf <- getDbConfig(dbName)
    drv <- RMariaDB::MariaDB()
    con <- DBI::dbConnect(
      drv,
      dbname = conf$name,
      host = conf$host,
      user = conf$user,
      password = conf$pass,
      bigint = "integer"
    )
    message("Connected to database: ", conf$name)
    # ensure utf8 encoding
    invisible(DBI::dbExecute(con, "SET NAMES utf8;"))
  } else if (dbType == "mssql") {
    conf <- getDbConfig(dbName)
    rlang::check_installed("odbc")
    drv <- odbc::odbc()
    con <- DBI::dbConnect(
      drv,
      driver = Sys.getenv("MYSQL_DRIVER", "FreeTDS"),
      database = conf$name,
      server = conf$host,
      TrustedConnection = "Yes",
      Timeout = 10
    )
    message("Connected to mssql-database: ", conf$name)
  } else if (dbType == "sqlite") {
    conf <- getDbConfig(dbName, sqlite = TRUE)
    rlang::check_installed("RSQLite")
    drv <- RSQLite::SQLite()
    con <- DBI::dbConnect(
      drv,
      dbname = conf$name
    )
  } else {
    stop(paste0("Unsupported dbType ", dbType, ". Use 'mysql' or 'sqlite'."))
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

#' Get database connection configuration
#'
#' @param dbName String providing the name of the database to connect to. If it
#' is "data" it will use the MYSQL_DB_DATA environment variable, if it is
#' "autoreport" it will use the MYSQL_DB_AUTOREPORT environment variable, and
#' if it is "raplog" it will use the MYSQL_DB_LOG environment variable. If
#' none of these are set, it will use the name provided.
#' @param sqlite A boolean indicating if the connection is to a SQLite database.
#' If TRUE, the envicronment variables MYSQL_HOST, MYSQL_USER and MYSQL_PASSWORD
#' are not needed.
#'
#' @return A list with name, user, password and host of the db connection.
#'
#' @keywords internal
#'
getDbConfig <- function(dbName = "data", sqlite = FALSE) {
  if (sqlite || (
    ("MYSQL_HOST" %in% names(Sys.getenv()))
    && ("MYSQL_USER" %in% names(Sys.getenv()))
    && ("MYSQL_PASSWORD" %in% names(Sys.getenv())
    )
  )) {
    conf <- data.frame(
      host = Sys.getenv("MYSQL_HOST"),
      user = Sys.getenv("MYSQL_USER"),
      pass = Sys.getenv("MYSQL_PASSWORD"),
      port = as.numeric(Sys.getenv("MYSQL_PORT", "3306"))
    )
    conf$name <- switch(
      dbName,
      "raplog" = Sys.getenv("MYSQL_DB_LOG"),
      "autoreport" = Sys.getenv("MYSQL_DB_AUTOREPORT"),
      "staging" = Sys.getenv("MYSQL_DB_STAGING"),
      "data" = Sys.getenv("MYSQL_DB_DATA"),
      dbName
    )
  } else {
    stop(paste0(
      "Could not connect to database because the enviroment
       variables MYSQL_HOST, MYSQL_USER and/or MYSQL_PASSWORD
       are not defined. Please check configuration."
    ))
  }
  return(conf)
}
