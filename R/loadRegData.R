#' Provider of data for registries at Rapporteket
#'
#' Generic to registries, provide reporting data obtained from sql databases
#' Underlying this function is rapbase::RapporteketDbConnection
#'
#' @param registryName String providing the name of the database to connect to.
#' If it is "data" it will use the MYSQL_DB_DATA environment variable, if it is
#' "autoreport" it will use the MYSQL_DB_AUTOREPORT environment variable, and
#' if it is "raplog" it will use the MYSQL_DB_LOG environment variable. If
#' none of these are set, it will use the name provided.
#' @param query String SQL query to obtain the data
#' @param dbType String Type of db to query, currently "mysql" (default) and
#' "mssql"
#' @param tabs Character vector for optional definition of tables to describe.
#' Defaults to an empty vector in which case all tables are used
#' @param tab String name of the table for which to get number of lines
#' @return data frame containing registry data or a list with table names and
#' corresponding fields with attributes
#' @name loadRegData
#' @aliases loadRegData describeRegistryDb
NULL


#' @rdname loadRegData
#' @export
loadRegData <- function(registryName = "data", query, dbType = "mysql") {
  dbList <- rapOpenDbConnection(registryName, dbType)
  regData <- DBI::dbGetQuery(dbList$con, query)
  rapCloseDbConnection(dbList$con)
  dbList <- NULL

  return(regData)
}

#' @rdname loadRegData
#' @export
describeRegistryDb <- function(registryName, tabs = c()) {

  desc <- list()

  if (length(tabs) == 0) {
    rawTable <- loadRegData(
      registryName = registryName,
      query = paste(
        "SELECT * FROM information_schema.columns",
        "WHERE table_schema = DATABASE();"
      )
    )
    for (tableName in unique(rawTable$TABLE_NAME)) {
      desc[[tableName]] <- rawTable |>
        dplyr::filter(.data$TABLE_NAME == tableName) |>
        dplyr::transmute(
          Field = .data$COLUMN_NAME,
          Type = .data$COLUMN_TYPE,
          Null = .data$IS_NULLABLE,
          Key = .data$COLUMN_KEY,
          Default = .data$COLUMN_DEFAULT,
          Extra = .data$EXTRA
        )
    }
  } else {
    for (tab in tabs) {
      query <- paste0("DESCRIBE ", tab, ";")
      desc[[tab]] <- loadRegData(registryName, query)
    }
  }
  desc
}

#' @rdname loadRegData
#' @export
nlinesRegistryDb <- function(registryName, tab = "") {

  query <- paste0(
    "SELECT COUNT(*) AS n_lines FROM ",
    tab, ";"
  )
  numLines <- 0
  tryCatch({
    numLines <- loadRegData(registryName, query)$n_lines
  } , error = function(e) {
    warning(paste0(
      "Number of lines in table ", tab,
      " could not be retrieved.\n",
      "Original error message:\n", e$message, ".\n"
    ))
  })
  numLines
}
