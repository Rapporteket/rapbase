# store current instance
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")
currentStagingKey <- Sys.getenv("MYSQL_DB_STAGING")

# test data
registryName <- "testReg"
dataName <- "testData"
d <- mtcars

# Define staging database name
Sys.setenv(MYSQL_DB_STAGING = "test_staging")

Sys.setenv(R_RAP_CONFIG_PATH = tempdir())

test_that("env vars needed for db testing is present", {
  check_db()
  expect_true("MYSQL_HOST" %in% names(Sys.getenv()))
  expect_true("MYSQL_USER" %in% names(Sys.getenv()))
  expect_true("MYSQL_PASSWORD" %in% names(Sys.getenv()))
})

test_that("No connection provided when no key (or connection object) given", {
  expect_error(dbStagingConnection(key = NULL, con = NULL))
})

test_that("dbStagingPrereq can not find db", {
  check_db()
  expect_error(
    rapbase:::dbStagingPrereq("staging"),
    "Failed to connect: Unknown database 'test_staging'"
  )
})

# prep db for testing
if (is.null(check_db(is_test_that = FALSE))) {
  create_staging_db(Sys.getenv("MYSQL_DB_STAGING"))
}

test_that("dbStagingPrereq can find db", {
  check_db()
  expect_invisible(rapbase:::dbStagingPrereq("staging"))
})

test_that("Data can be staged with db backend", {
  check_db()
  d0 <- saveStagingData(registryName, "testData", d)
  expect_true(identical(d, d0))
})

test_that("staging files can be listed from db backend", {
  check_db()
  v <- listStagingData(registryName)
  expect_equal(class(v), "character")
  expect_identical(v, "testData")
})

test_that("modification time of stagin data in db can be obtained", {
  check_db()
  expect_true("POSIXct" %in% class(mtimeStagingData(registryName)))
})

test_that("retrieval of none existing data returns FALSE", {
  check_db()
  expect_false(loadStagingData(registryName, "noSuchDataSet"))
})

test_that("data can be retrieved from staging db", {
  check_db()
  expect_equal(loadStagingData(registryName, dataName), d)
})

test_that("deleting a none-existing dataset from db returns FALSE", {
  check_db()
  expect_false(deleteStagingData(registryName, "imaginaryDataSet"))
})

test_that("a dataset can be deleted from db", {
  check_db()
  expect_true(deleteStagingData(registryName, dataName))
  expect_false(loadStagingData(registryName, dataName))
})

test_that("a global clean of db staging data can be performed (also dry run)", {
  check_db()
  expect_equal(saveStagingData(registryName, dataName, d), d)
  expect_identical(listStagingData(registryName), dataName)
  expect_message(cleanStagingData(0))
  expect_equal(class(cleanStagingData(0)), "character")
  expect_identical(listStagingData(registryName), dataName)
  expect_invisible(cleanStagingData(0, dryRun = FALSE))
  expect_false(loadStagingData(registryName, dataName))
})

if (is.null(check_db(is_test_that = FALSE))) {
  query <- paste0("DROP DATABASE ", Sys.getenv("MYSQL_DB_STAGING"), ";")
  query_db(query)
}

# Restore environment
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
Sys.setenv(MYSQL_DB_STAGING = currentStagingKey)
