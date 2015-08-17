#' Provider of data for registries at Rapporteket
#'
#' Generic to registries, provide reporting data obtained from sql databases
#' Underlying this function is rapbase::RapporteketDbConnection
#' 
#' @param registryName String Name of the registry as defined in dbConfig.yml
#' @param query String SQL query to obtain the data
#' @param dbType String Type of db to query, currently "mysql" (default) and "mssql"
#' @return RegData dataframe Registry data
#' @export

LoadRegData <- function(registryName, query, dbType = "mysql") {
  
  dbList <- rapbase::rapOpenDbConnection(registryName, dbType)
  RegData <- DBI::dbGetQuery(dbList$con, query)
  rapbase::rapCloseConnection(dbList$con, dbList$drv)
  dbList <- NULL
  
  return(RegData)
}