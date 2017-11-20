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
  
  dbList <- rapOpenDbConnection(registryName, dbType)
  if (registryName == "nkr"){ # nocov start
    # ugly hack to get past 'out of heap mem' for nkr
    res <- DBI::dbSendQuery(dbList$con, query)
    RegData <- DBI::dbFetch(res, n = 20000)
    RegData <- rbind(RegData, DBI::dbFetch(res, n = -1))
    tmp <- DBI::dbClearResult(res)
    # nocov end
  } else {
    RegData <- DBI::dbGetQuery(dbList$con, query)
  }
  rapCloseDbConnection(dbList$con)
  dbList <- NULL
  
  return(RegData)
}