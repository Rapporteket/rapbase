#' Provide connection handle for data source at Rapporteket
#'
#' Generic to registries, handle the data source connections, including
#' usernames and passwords needed to open these connections
#'
#' @param registryName String id used for the registry in global configuration
#'   file from which information on the database connection is provided
#' @param dbType String providing type of data source, one of
#'   "mysql" and "mssql". Defaults to "mysql"
#' @return A named list of con and drv representing the db connection handle and
#'   driver, respectively.
#' @export

rapOpenDbConnection <- function(registryName, dbType = "mysql") {

  if (dbType == "mysql") {
    conf <- getDbConfig(registryName)
    drv <- RMariaDB::MariaDB()
    con <- DBI::dbConnect(
      drv,
      dbname = conf$name,
      host = conf$host,
      user = conf$user,
      password = conf$pass,
      bigint = "integer"
    )
    # ensure utf8 encoding
    invisible(DBI::dbExecute(con, "SET NAMES utf8;"))
  } else if (dbType == "mssql") {
    stop("Use of MSSQL is no longer supported. Exiting")
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
#' @param registryName String id used for the registry in global configuration
#'   file from which information on the database connection is provided
#'
#' @return A list with name, user, password and host of the db connection.
#'
#' @keywords internal
#'
getDbConfig <- function(registryName = "data") {
  if (
    ("MYSQL_HOST" %in% names(Sys.getenv())) &&
      ("MYSQL_USER" %in% names(Sys.getenv())) &&
      ("MYSQL_PASSWORD" %in% names(Sys.getenv()))
  ) {
    conf <- data.frame(
      host = Sys.getenv("MYSQL_HOST"),
      user = Sys.getenv("MYSQL_USER"),
      pass = Sys.getenv("MYSQL_PASSWORD"),
      port = as.numeric(Sys.getenv("MYSQL_PORT", "3306"))
    )
    conf$name <- switch(
      registryName,
      "raplog" = Sys.getenv("MYSQL_DB_LOG"),
      "autoreport" = Sys.getenv("MYSQL_DB_AUTOREPORT"),
      "data" = Sys.getenv("MYSQL_DB_DATA"),
      registryName
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
