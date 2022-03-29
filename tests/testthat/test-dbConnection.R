context("Handling db connections")

# For these test to work locally make sure an instance of mysql server is
# running and that the necessary user privileges are provided, e.g. as SQL:
#   grant all privileges on [DATABASE].* to '[USER]'@'localhost';
# where [DATABASE] and [USER] correspond to whatever given in rapbase config.
#
# When run at Travis build servers [USER] must be set to 'travis' and with
# an empty password (as also assumed in the above localhost example). See also
# .travis.yml

# Database infrastructure is only available at GA and our own dev env.
# Tests running on other environments should be skipped
checkDb <- function(is_test_that = TRUE) {
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

test_that("Error provided when key has no corresponding config", {
  NULL
  expect_error(rapOpenDbConnection(registryName = "aNoneExistingRegistryKey"))
})


test_that("env vars needed for testing is present", {
  checkDb()
  expect_true("DB_HOST" %in% names(Sys.getenv()))
  expect_true("DB_USER" %in% names(Sys.getenv()))
  expect_true("DB_PASS" %in% names(Sys.getenv()))
})

# prep db for testing
if (is.null(checkDb(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    bigint = "integer"
  )
  RMariaDB::dbExecute(con, "CREATE DATABASE rapbase;")
  RMariaDB::dbDisconnect(con)
}

if (is.null(checkDb(is_test_that = FALSE))) {
  regName <- "dev"
  query <- c(
    "USE rapbase;",
    paste(
      "CREATE TABLE testTable (id int, someText varchar(50),",
      "someInt INT, someBigInt BIGINT, someFloat DOUBLE,",
      "someTime DATETIME);"
    )
  )
  drv <- RMariaDB::MariaDB()
  con <- RMariaDB::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    bigint = "integer"
  )
  for (q in query) {
    tmp <- DBI::dbExecute(con, q)
  }
  DBI::dbDisconnect(con)
}

# make temporary config
regName <- "rapbase"
test_config <- paste0(
  "rapbase:",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : rapbase",
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : ephemaralUnitTesting\n"
)
# preserve initial state
config_path <- Sys.getenv("R_RAP_CONFIG_PATH")
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)

test_that("A mysql db connection and driver can be provided and cleaned", {
  checkDb()
  l <- rapOpenDbConnection(registryName = regName)
  expect_output(str(l), "List of 2")
  expect_is(l[[1]], "MariaDBConnection")
  expect_is(l[[2]], "MariaDBDriver")
  expect_true(RMariaDB::dbIsValid(l$con))
  rapCloseDbConnection(l$con)
  expect_false(RMariaDB::dbIsValid(l$con))
})

test_that("Deprecated defunct interface provides an error", {
  checkDb()
  query <- "SELECT * FROM testTable"
  expect_error(LoadRegData(regName, query, dbType = "mysql"))
})

test_that("Data can be queried from (MySQL) db", {
  checkDb()
  query <- "SELECT * FROM testTable"
  expect_output(
    str(loadRegData(regName, query, dbType = "mysql")),
    "data.frame"
  )
})

test_that("metadata can be queried from db", {
  checkDb()
  expect_equal(
    class(describeRegistryDb(regName)),
    "list"
  )
})

test_that("metadata can be queried from some tabs in db", {
  checkDb()
  expect_equal(
    class(describeRegistryDb(regName, tabs = c("testTable"))),
    "list"
  )
})

test_that("Bigints are returned as integers (not bit64::integer64)", {
  checkDb()
  query <- c(
    "DROP DATABASE IF EXISTS rapbase;",
    "CREATE DATABASE rapbase;",
    "USE rapbase;",
    paste(
      "CREATE TABLE testTable (id int, someText varchar(50),",
      "someInt INT, someBigInt BIGINT, someFloat DOUBLE,",
      "someTime DATETIME);"
    )
  )
  l <- rapOpenDbConnection(regName)
  for (q in query) {
    tmp <- DBI::dbExecute(l$con, q)
  }
  query <- "SELECT * FROM testTable;"
  df <- DBI::dbGetQuery(l$con, query)
  rapCloseDbConnection(l$con)
  expect_is(df[["someBigInt"]], "integer")
})

test_that(paste(
  "The use of MSSQL in no longer possible with an appropriate",
  "message"
), {
  expect_error(loadRegData(regName, query, dbType = "mssql"),
    regexp = "Use of MSSQL is no longer supported. Exiting"
  )
})

# remove test db
if (is.null(checkDb(is_test_that = FALSE))) {
  con <- rapbase::rapOpenDbConnection(regName)$con
  RMariaDB::dbExecute(con, "DROP DATABASE rapbase;")
  rapbase::rapCloseDbConnection(con)
}

# restore initial state
Sys.setenv(R_RAP_CONFIG_PATH = config_path)
