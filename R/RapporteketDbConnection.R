#' Provide connection handle for data source at Rapporteket
#' 
#' Generic to registries, handle the data source connections, including
#' usernames and passwords needed to open these connections
#' 
#' @param registryName String id used for the registry in global configuration
#'  file from which informastion on the database connection is provided
#' @param dbType String providing type of data source, one of
#'  "mysql" and "mssql". Defaults to "mysql"
#' @return con Data source connection object
#' @return drv DBIDriver object
#' @export 

rapOpenDbConnection <- function(registryName, dbType = "mysql") {
  
  conf <- getConfig()
  conf <- conf[[registryName]]
  if (dbType == "mysql") {
    drv <- RMySQL::MySQL()
    con <- DBI::dbConnect(drv,
                          dbname = conf$name,
                          host = conf$host,
                          user = conf$user,
                          password = conf$pass)
    # ensure utf8 encoding
    tmp <- DBI::dbGetQuery(con, "SET NAMES utf8;")
  }
  else if (dbType == "mssql") {
    
    drv <- RJDBC::JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
                       system.file("sqljdbc4.jar", package = "rapbase"))
    dbUrl <- paste("jdbc:sqlserver://", conf$host, ":", conf$port,
                   ";databaseName=", conf$nkr$name,
                   ";instance=", conf$nkr$inst, ";charset=UTF-8", sep="")
    con <- DBI::dbConnect(drv, dbUrl, user = conf$user, password = conf$pass)
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