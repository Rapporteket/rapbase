#' Provide connection handle for data source at Rapporteket
#' 
#' Generic to registries, handle the data source connections, including
#' usernames and passwords needed to open these connections
#' 
#' @param registryName String id used for the reistry from which data
#'  connection is provided
#' @param dbType String providing type of data source, one of
#'  "mysql" and "mssql". Defaults to "mysql"
#' @return con Data source connection object
#' @return drv DBIDriver object
#' @export 

rapOpenDbConnection <- function(registryName, dbType = "mysql") {
  
  conf <- yaml::yaml.load_file(system.file("dbConfig.yml", package = "rapbase"))
  conf <- conf[[registryName]]
  if (dbType == "mysql") {
    drv <- DBI::dbDriver("MySQL")
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
                system.file("sqljdbc4.jar", package = "repbase"))
    dbUrl <- paste("jdbc:sqlserver://", conf$host, ":", conf$port,
                   ";databaseName=", conf$nkr$name,
                   ";instance=", conf$nkr$inst, ";charset=UTF-8", sep="")
    con <- DBI::dbConnect(drv, dbUrl, user = conf$user, password = conf$pass)
  }
  
  return(con, drv)
}


#' Close down data connection handle
#' 
#' @param con Open connection object that is to be closed
#' @param drv DBIDriver object that is to be removed
#' @export

rapCloseDbConnection <- function(con, drv) {
  con <- DBI::dbDisconnect(con)
  con <- DBI::dbUnloadDriver(drv)
  con <- NULL
}