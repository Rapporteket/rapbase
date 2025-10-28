# Database infrastructure is only guaranteed at Github Actions and our own
# dev env.
# Tests running on other environments should be skipped:
check_db <- function(is_test_that = TRUE) {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    if (is_test_that) {
      testthat::skip("Possible lack of database infrastructure")
    } else {
      1
    }
  }
}

# Send a query to db
query_db <- function(query, ...) {
  if (is.null(check_db(is_test_that = FALSE))) {
    con <- connect_db(...)
    for (q in query) {
      DBI::dbExecute(con, q)
    }
    DBI::dbDisconnect(con)
    con <- NULL
  }
}

# Connect to database server
connect_db <- function(...) {
  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("MYSQL_HOST"),
    user = Sys.getenv("MYSQL_USER"),
    password = Sys.getenv("MYSQL_PASSWORD"),
    bigint = "integer",
    ...
  )
}

close_db_connection <- function(con) {
  con <- DBI::dbDisconnect(con)
  con <- NULL
}

create_log_db <- function(dbLogKey) {

  fc <- file(system.file("createRaplogTabs.sql", package = "rapbase"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries <- c(
    paste0("DROP DATABASE IF EXISTS ", dbLogKey, ";"),
    paste0("CREATE DATABASE ", dbLogKey, ";"),
    paste0("USE ", dbLogKey, ";"),
    strsplit(sql, ";")[[1]]
  )
  query_db(query = queries)

}

create_staging_db <- function(dbStagingKey) {

  fc <- file(system.file("createStagingTab.sql", package = "rapbase"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries <- c(
    paste0("DROP DATABASE IF EXISTS ", dbStagingKey, ";"),
    paste0("CREATE DATABASE ", dbStagingKey, ";"),
    paste0("USE ", dbStagingKey, ";"),
    strsplit(sql, ";")[[1]]
  )
  query_db(query = queries)
}
