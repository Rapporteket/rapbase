# Database infrastructure is only guaranteed at Github Actions and our own
# dev env.
# Tests running on other environments should be skipped:
check_db <- function(is_test_that = TRUE) {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
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
query_db <- function(query) {
  if (is.null(check_db(is_test_that = FALSE))) {
    con <- RMariaDB::dbConnect(
      RMariaDB::MariaDB(),
      host = Sys.getenv("MYSQL_HOST"),
      user = Sys.getenv("MYSQL_USER"),
      password = Sys.getenv("MYSQL_PASSWORD"),
      bigint = "integer"
    )
    for (q in query) {
      RMariaDB::dbExecute(con, q)
    }
    RMariaDB::dbDisconnect(con)
    con <- NULL
  }
}
