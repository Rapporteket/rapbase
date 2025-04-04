context("Handling db connections")

# For these test to work locally make sure an instance of mysql server is
# running and that the necessary user privileges are provided, e.g. as SQL:
#   grant all privileges on [DATABASE].* to '[USER]'@'localhost';
# where [DATABASE] and [USER] correspond to whatever given in rapbase config.
#
# When run at Travis build servers [USER] must be set to 'travis' and with
# an empty password (as also assumed in the above localhost example). See also
# .travis.yml

test_that("Error provided when key has no corresponding config", {
  NULL
  expect_error(rapOpenDbConnection(registryName = "aNoneExistingRegistryKey"))
})


test_that("env vars needed for testing is present", {
  check_db()
  expect_true("MYSQL_HOST" %in% names(Sys.getenv()))
  expect_true("MYSQL_USER" %in% names(Sys.getenv()))
  expect_true("MYSQL_PASSWORD" %in% names(Sys.getenv()))
})

# prep db for testing
if (is.null(check_db(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("MYSQL_HOST"),
    user = Sys.getenv("MYSQL_USER"),
    password = Sys.getenv("MYSQL_PASSWORD"),
    bigint = "integer"
  )
  RMariaDB::dbExecute(con, "CREATE DATABASE rapbase;")
  RMariaDB::dbDisconnect(con)
}

if (is.null(check_db(is_test_that = FALSE))) {
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
    host = Sys.getenv("MYSQL_HOST"),
    user = Sys.getenv("MYSQL_USER"),
    password = Sys.getenv("MYSQL_PASSWORD"),
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
  "\n  host : ", Sys.getenv("MYSQL_HOST"),
  "\n  name : rapbase",
  "\n  user : ", Sys.getenv("MYSQL_USER"),
  "\n  pass : ", Sys.getenv("MYSQL_PASSWORD"),
  "\n  disp : ephemaralUnitTesting\n"
)
# preserve initial state
config_path <- Sys.getenv("R_RAP_CONFIG_PATH")
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)

test_that("A mysql db connection and driver can be provided and cleaned", {
  check_db()
  l <- rapOpenDbConnection(registryName = regName)
  expect_output(str(l), "List of 2")
  expect_is(l[[1]], "MariaDBConnection")
  expect_is(l[[2]], "MariaDBDriver")
  expect_true(RMariaDB::dbIsValid(l$con))
  rapCloseDbConnection(l$con)
  expect_false(RMariaDB::dbIsValid(l$con))
  l <- NULL
})

test_that("Deprecated defunct interface provides an error", {
  check_db()
  query <- "SELECT * FROM testTable"
  expect_error(LoadRegData(regName, query, dbType = "mysql"))
})

test_that("Data can be queried from (MySQL) db", {
  check_db()
  query <- "SELECT * FROM testTable"
  expect_output(
    str(loadRegData(regName, query, dbType = "mysql")),
    "data.frame"
  )
})

test_that("metadata can be queried from db", {
  check_db()
  expect_equal(
    class(describeRegistryDb(regName)),
    "list"
  )
})

test_that("metadata can be queried from some tabs in db", {
  check_db()
  expect_equal(
    class(describeRegistryDb(regName, tabs = c("testTable"))),
    "list"
  )
})

test_that("Bigints are returned as integers (not bit64::integer64)", {
  check_db()
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
  l <- NULL
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


withr::with_envvar(
  new = c(
    "MYSQL_HOST" = NA,
    "MYSQL_USER" = NA,
    "MYSQL_PASSWORD" = NA
  ),
  code = {
    test_that("getDbConfig is working when not db", {
      expect_error(
        getDbConfig(),
        regexp = "configuration corresponding to key 'data'."
      )
      expect_error(
        getDbConfig(registryName = "dev"),
        regexp = "configuration corresponding to key 'dev'."
      )
      expect_equal(
        getDbConfig(registryName = "rapbase")$name,
        "rapbase"
      )
    })
  }
)


withr::with_envvar(
  new = c(
    "MYSQL_HOST" = "qwerty",
    "MYSQL_USER" = "asdfg",
    "MYSQL_PASSWORD" = "zxcvb",
    "MYSQL_DB_LOG" = "log_db",
    "MYSQL_DB_AUTOREPORT" = "autoreport_db",
    "MYSQL_DB_DATA" = "data_db",
    "rapbase" = "data_db_rapbase"
  ),
  code = {
    test_that("Returns default values", {
      conf <- getDbConfig()
      expect_equal(conf$host, "qwerty")
      expect_equal(conf$name, "data_db")
      expect_equal(conf$user, "asdfg")
      expect_equal(conf$pass, "zxcvb")
      expect_equal(conf$port, 3306)

      conf <- getDbConfig(registryName = "rapbase")
      expect_equal(conf$name, "rapbase")

      conf <- getDbConfig(registryName = "raplog")
      expect_equal(conf$name, "log_db")

      conf <- getDbConfig(registryName = "autoreport")
      expect_equal(conf$name, "autoreport_db")

      conf <- getDbConfig(registryName = "data")
      expect_equal(conf$name, "data_db")

      conf <- getDbConfig(registryName = "qwerty123")
      expect_equal(conf$name, "qwerty123")
      expect_equal(conf$host, "qwerty")
      expect_equal(conf$user, "asdfg")
      expect_equal(conf$pass, "zxcvb")
      expect_equal(conf$port, 3306)
    })
  }
)

# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  con <- rapbase::rapOpenDbConnection(regName)$con
  RMariaDB::dbExecute(con, "DROP DATABASE rapbase;")
  rapbase::rapCloseDbConnection(con)
  con <- NULL
}

# restore initial state
Sys.setenv(R_RAP_CONFIG_PATH = config_path)
