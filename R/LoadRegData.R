#' Provider of data for registries at Rapporteket
#'
#' Generic to registries, provide reporting data obtained from sql databases
#' Underlying this function is rapbase::RapporteketDbConnection
#'
#' @param registryName String Name of the registry as defined in dbConfig.yml
#' @param query String SQL query to obtain the data
#' @param dbType String Type of db to query, currently "mysql" (default) and
#' "mssql"
#' @return regData data frame containing registry data
#' @name loadRegData
#' @aliases LoadRegData loadRegData
NULL

#' @rdname loadRegData
#' @export
LoadRegData <- function(registryName, query, dbType = "mysql") {

  lifecycle::deprecate_warn("1.12.0", "rapbase::LoadRegData()",
                            "rapbase::loadRegData()")

  dbList <- rapOpenDbConnection(registryName, dbType)
  if (registryName == "nkr") { # nocov start
    # ugly hack to get past 'out of heap mem' for nkr
    res <- DBI::dbSendQuery(dbList$con, query)
    regData <- DBI::dbFetch(res, n = 20000)
    regData <- rbind(regData, DBI::dbFetch(res, n = -1))
    tmp <- DBI::dbClearResult(res)
    # nocov end
  } else {
    regData <- DBI::dbGetQuery(dbList$con, query)
  }
  rapCloseDbConnection(dbList$con)
  dbList <- NULL

  return(regData)
}

#' @rdname loadRegData
#' @export
loadRegData <- function(registryName, query, dbType = "mysql") {

  dbList <- rapOpenDbConnection(registryName, dbType)
  regData <- DBI::dbGetQuery(dbList$con, query)
  rapCloseDbConnection(dbList$con)
  dbList <- NULL

  return(regData)
}
