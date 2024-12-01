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
  if (Sys.getenv("R_RAP_INSTANCE") %in% c("QAC", "PRODUCTIONC")) {
    conf <- data.frame(
      host = Sys.getenv("MYSQL_HOST"),
      user = Sys.getenv("MYSQL_USER"),
      password = Sys.getenv("MYSQL_PASSWORD"),
      port = as.numeric(Sys.getenv("MYSQL_PORT", "3306"))
    )
    conf$name <- switch(
      registryName,
      "raplog" = Sys.getenv("MYSQL_DB_LOG"),
      "autoreport" = Sys.getenv("MYSQL_DB_AUTOREPORT"),
      "data" = Sys.getenv("MYSQL_DB_DATA"),
      Sys.getenv(registryName)
    )
  } else {
    conf <- getConfig()
    conf <- conf[[registryName]]
    if (is.null(conf)) {
      stop(paste0(
        "Could not connect to database because there is no
                configuration corresponding to key '", registryName,
        "'. Please check key and/or configuration."
      ))
    }
  }

  if (dbType == "mysql") {
    drv <- RMariaDB::MariaDB()
    con <- DBI::dbConnect(
      drv,
      dbname = conf$name,
      host = conf$host,
      user = conf$user,
      password = conf$pass,
      bigint = "integer",
      client.flag = 2048,  # Request SSL connection
      ssl.verify = FALSE         # Disable SSL verification
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
