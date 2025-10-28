#' Create a Database
#'
#' This function creates a new database with the specified name and type.
#'
#' @param dbName A character string specifying the name of the database to be
#' created.
#' @param dbType A character string specifying the type of the database.
#'   Default is `"mysql"`. Other supported types may depend on the
#' implementation.
#' @param force A logical value indicating whether to overwrite an existing
#' database with the same name. Default is `FALSE`.
#'
#' @return The function does not return a value but creates a database as a
#' side effect.
#'
#' @examples
#' \dontrun{
#' createDb("myDatabase") # Creates a MySQL database named "myDatabase"
#' createDb("myDatabase", dbType = "sqlite") # Creates an SQLite database
#' createDb("myDatabase", force = TRUE) # Forces creation, overwriting if exists
#' }
#'
#' @export
createDb <- function(dbName, dbType = "mysql", force = FALSE) {
  if (Sys.getenv("db_type") != "") {
    dbType <- Sys.getenv("db_type")
  }
  if (dbType == "mysql") {
    con <- DBI::dbConnect(
      RMariaDB::MariaDB(),
      host = Sys.getenv("MYSQL_HOST"),
      user = Sys.getenv("MYSQL_USER"),
      password = Sys.getenv("MYSQL_PASSWORD"),
      bigint = "integer"
    )
    if (force) {
      DBI::dbExecute(con, paste0("DROP DATABASE IF EXISTS ", dbName, ";"))
    }
    DBI::dbExecute(con, paste0("CREATE DATABASE IF NOT EXISTS ", dbName, ";"))
    invisible(DBI::dbDisconnect(con))
  } else if (dbType == "sqlite") {
    # will create a file named dbName in the working directory
    con <- DBI::dbConnect(RSQLite::SQLite(), dbName)
  } else {
    stop(paste0("Unsupported dbType ", dbType, ". Use 'mysql' or 'sqlite'."))
  }
  invisible(DBI::dbDisconnect(con))
}
