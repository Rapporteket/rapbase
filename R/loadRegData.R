#' Provider of data for registries at Rapporteket
#'
#' Generic to registries, provide reporting data obtained from sql databases
#' Underlying this function is rapbase::RapporteketDbConnection
#'
#' @param registryName String Name of the registry as defined in dbConfig.yml
#' @param query String SQL query to obtain the data
#' @param dbType String Type of db to query, currently "mysql" (default) and
#' "mssql"
#' @param tabs Character vector for optional definition of tables to describe.
#' Defaults to an empty vector in which case all tables are used
#' @return data frame containing registry data or a list with table names and
#' corresponding fields with attributes
#' @name loadRegData
#' @aliases loadRegData describeRegistryDb
NULL


#' @rdname loadRegData
#' @export
loadRegData <- function(registryName, query, dbType = "mysql") {
  dbList <- rapOpenDbConnection(registryName, dbType)
  regData <- DBI::dbGetQuery(dbList$con, query)
  rapCloseDbConnection(dbList$con)
  dbList <- NULL

  return(regData)
}

#' @rdname loadRegData
#' @export
describeRegistryDb <- function(registryName, tabs = c()) {
  qGetTabs <- "SHOW TABLES;"
  qGetDesc <- "DESCRIBE "

  desc <- list()

  if (length(tabs) == 0) {
    tabs <- rapbase::loadRegData(
      registryName = registryName,
      query = qGetTabs
    )[[1]]
  }

  for (tab in tabs) {
    query <- paste0(qGetDesc, tab, ";")
    desc[[tab]] <- loadRegData(registryName, query)
  }

  desc
}
