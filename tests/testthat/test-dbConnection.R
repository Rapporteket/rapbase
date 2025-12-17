context("Handling db connections")

# For these test to work locally make sure an instance of mysql server is
# running and that the necessary user privileges are provided, e.g. as SQL:
#   grant all privileges on [DATABASE].* to '[USER]'@'[HOST]';
# where [DATABASE], [USER] and [HOST] correspond to whatever given in
# environment variables MYSQL_NAME, MYSQL_USER and MYSQL_HOST.
#

test_that("Error provided when key has no corresponding config", {
  NULL
  expect_error(rapOpenDbConnection(dbName = "aNoneExistingRegistryKey"))
})

regName <- "rapbase"

test_that("env vars needed for testing is present", {
  check_db()
  expect_true("MYSQL_HOST" %in% names(Sys.getenv()))
  expect_true("MYSQL_USER" %in% names(Sys.getenv()))
  expect_true("MYSQL_PASSWORD" %in% names(Sys.getenv()))
})

# prep db for testing
query <- c(
  paste0("DROP DATABASE IF EXISTS ", regName, ";"),
  paste0("CREATE DATABASE ", regName, ";")
)
query_db(query = query)

# Create simple test table
query <- c(
  paste0("USE ", regName, ";"),
  paste(
    "CREATE TABLE testtable (id int, someText varchar(50),",
    "someInt INT, someBigInt BIGINT, someFloat DOUBLE,",
    "someTime DATETIME);"
  )
)
query_db(query = query)


if (is.null(check_db(is_test_that = FALSE))) {
  # add some data to db
  con <- rapOpenDbConnection(regName)$con
  DBI::dbAppendTable(con, "testtable", testdata, row.names = NULL)
  rapCloseDbConnection(con)
}

test_that("A mysql db connection and driver can be provided and cleaned", {
  check_db()
  l <- rapOpenDbConnection(dbName = regName)
  expect_output(str(l), "List of 2")
  expect_is(l[[1]], "MariaDBConnection")
  expect_is(l[[2]], "MariaDBDriver")
  expect_true(DBI::dbIsValid(l$con))
  rapCloseDbConnection(l$con)
  expect_false(DBI::dbIsValid(l$con))
  l <- NULL
})

test_that("Data can be queried from (MySQL) db", {
  check_db()
  query <- "SELECT * FROM testtable"
  expect_equal(
    loadRegData(regName, query, dbType = "mysql")$id,
    c(1:10)
  )
  expect_equal(
    loadRegData(regName, query, dbType = "mysql")$someInt,
    c(11:20)
  )
})

test_that("Data can be queried from (MySQL) db with no data", {
  check_db()
  query <- "SELECT * FROM testtable WHERE id = -1"
  expect_equal(
    length(loadRegData(regName, query, dbType = "mysql")$id),
    0
  )
})

test_that("metadata can be queried from db", {
  check_db()
  testSet <- describeRegistryDb(regName)
  expect_equal(
    class(testSet),
    "list"
  )
  expect_equal(
    names(testSet$testtable),
    c("Field", "Type", "Null", "Key", "Default", "Extra")
  )
  expect_equal(
    testSet$testtable$Type,
    c("int", "varchar(50)", "int", "bigint", "double", "datetime")
  )
})

test_that("metadata can be queried from some tabs in db", {
  check_db()
  testSet <- describeRegistryDb(regName, tabs = c("testtable"))
  expect_equal(
    class(testSet),
    "list"
  )
  expect_equal(
    names(testSet$testtable),
    c("Field", "Type", "Null", "Key", "Default", "Extra")
  )
  expect_equal(
    testSet$testtable$Type,
    c("int", "varchar(50)", "int", "bigint", "double", "datetime")
  )
})

test_that("number of lines can be queried from some tabs in db", {
  check_db()
  expect_equal(
    nlinesRegistryDb(regName, tab = "testtable"),
    10
  )
  expect_warning(
    nlinesRegistryDb(regName, tab = "wrongtesttable"),
    "Number of lines in table wrongtesttable could not be retrieved"
  )
})

test_that("Bigints are returned as integers (not bit64::integer64)", {
  check_db()
  query <- c(
    "DROP DATABASE IF EXISTS rapbase;",
    "CREATE DATABASE rapbase;",
    "USE rapbase;",
    paste(
      "CREATE TABLE testtable (id int, someText varchar(50),",
      "someInt INT, someBigInt BIGINT, someFloat DOUBLE,",
      "someTime DATETIME);"
    )
  )
  l <- rapOpenDbConnection(regName)
  for (q in query) {
    tmp <- DBI::dbExecute(l$con, q)
  }
  query <- "SELECT * FROM testtable;"
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

test_that(paste(
  "The use of unknown dbType return an appropriate",
  "message"
), {
  expect_error(loadRegData(regName, query, dbType = "qwertysql"),
               regexp = "Unsupported dbType qwertysql. Use 'mysql' or 'sqlite'."
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
        regexp = "Could not connect to database because the enviroment"
      )
      expect_error(
        getDbConfig(dbName = "dev"),
        regexp = "Could not connect to database because the enviroment"
      )
      expect_error(
        getDbConfig(dbName = "rapbase")$name,
        regexp = "Could not connect to database because the enviroment"
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

      conf <- getDbConfig(dbName = "rapbase")
      expect_equal(conf$name, "rapbase")

      conf <- getDbConfig(dbName = "raplog")
      expect_equal(conf$name, "log_db")

      conf <- getDbConfig(dbName = "autoreport")
      expect_equal(conf$name, "autoreport_db")

      conf <- getDbConfig(dbName = "data")
      expect_equal(conf$name, "data_db")

      conf <- getDbConfig(dbName = "qwerty123")
      expect_equal(conf$name, "qwerty123")
      expect_equal(conf$host, "qwerty")
      expect_equal(conf$user, "asdfg")
      expect_equal(conf$pass, "zxcvb")
      expect_equal(conf$port, 3306)
    })
  }
)

# remove test db
query_db(paste0("DROP DATABASE IF EXISTS ", regName, ";"))
