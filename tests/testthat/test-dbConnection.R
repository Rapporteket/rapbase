context("Handling db connections")

# For these test to work locally make sure an instance of mysql server is
# running and that the necassary user privileges are provided, e.g. as SQL:
#   grant all privileges on [DATABASE].* to '[USER]'@'localhost';
# where [DATABASE] and [USER] correspond to whatever given in rapbase config:
#   conf <- rapbase::getConf()
# When run at Travis build servers [USER] must be set to 'travis' and with
# an empty password (as also assumed in the above localhost example). See also
# .travis.yml

# Database infrastructure is only available at Travis and our own dev env.
# Tests running on other environments should be skipped
checkDb <- function() {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("TRAVIS") == "true") {
    NULL
  } else {
    skip("Test skipped due to lack of database infrastructure")
  }
}

test_that("Error provided when key has no corresponding config", {
  NULL
  expect_error(rapOpenDbConnection(registryName = "aNoneExistingRegistryKey"))
})


# Make sure we do have a test db during DEV
# On Travis, this will be taken care of in config (.travis.yml)
# If in a DEV context alter the default travis setup
regName <- "rapbase"
if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
  regName <- "dev"
  query <- c("DROP DATABASE IF EXISTS rapbase;",
             "CREATE DATABASE rapbase;",
             "USE rapbase;",
             paste("CREATE TABLE testTable (id int, someText varchar(50),",
                   "someInt INT, someBigInt BIGINT, someFloat DOUBLE,",
                   "someTime DATETIME);"))
  conf <- getConfig()[[regName]]
  drv <- RMariaDB::MariaDB()
  con <- DBI::dbConnect(drv,
                        host = conf$host,
                        user = conf$user,
                        password = conf$pass)
  for (q in query) {
    tmp <- DBI::dbExecute(con, q)
  }
  DBI::dbDisconnect(con)
}

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

test_that("Data can be queried from (MySQL) db", {
  checkDb()
  query <- "SELECT * FROM testTable"
  expect_output(str(LoadRegData(regName, query, dbType = "mysql")),
                "data.frame")
})

test_that("Bigints are returned as integers (not bit64::integer64)", {
  checkDb()
  query <- c("DROP DATABASE IF EXISTS rapbase;",
             "CREATE DATABASE rapbase;",
             "USE rapbase;",
             paste("CREATE TABLE testTable (id int, someText varchar(50),",
             "someInt INT, someBigInt BIGINT, someFloat DOUBLE,",
             "someTime DATETIME);"))
  l <- rapOpenDbConnection(regName)
  for (q in query) {
    tmp <- DBI::dbExecute(l$con, q)
  }
  query <- "SELECT * FROM testTable;"
  df <- DBI::dbGetQuery(l$con, query)
  rapCloseDbConnection(l$con)
  expect_is(df[["someBigInt"]], "integer")
})

test_that("The use of MSSQL in no longer possible with an appropriate message", {
  expect_error(LoadRegData(regName, query, dbType = "mssql"),
                regexp = "Use of MSSQL is no longer supported. Exiting")
})
