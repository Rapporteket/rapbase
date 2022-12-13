# store current instance
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

# test data
registryName <- "testReg"
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
dataName <- "testData"
d <- mtcars
testPath <- file.path(
  Sys.getenv("R_RAP_CONFIG_PATH"),
  "stagingData",
  registryName
)
testFile <- file.path(testPath, dataName)

# test config for file backend
test_config <- paste0(
  "r:",
  "\n  staging: ",
  "\n    target: file",
  "\n    key: staging\n"
)
cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml"))
writeLines(test_config, cf)
close(cf)

# make proper dbConfig
test_config <- paste0(
  registryName, ":",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : test_staging",
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : registryEphemaralUnitTesting\n",
  "staging:",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : test_staging",
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : dbBackendEphemaralUnitTesting\n"
)
cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)

test_that("staging cannot commence if parent directory does not exist", {
  expect_error(pathStagingData(registryName, dir = "imaginaryDir"))
  expect_error(
    saveStagingData(registryName, "testData", d, dir = "imaginaryDir")
  )
})

test_that("relevant directories are created at first time use", {
  expect_equal(
    pathStagingData(registryName,
      dir = Sys.getenv("R_RAP_CONFIG_PATH")
    ),
    testPath
  )
})

test_that("data can be stored for staging", {
  expect_equal(saveStagingData(registryName, dataName, d), d)
  expect_true(file.exists(testFile))
})

test_that("staging files can be listed", {
  expect_equal(class(listStagingData(registryName)), "character")
})

test_that("modification time of stagin files can be obtained", {
  expect_true("POSIXct" %in% class(mtimeStagingData(registryName)))
})

test_that("data can be retrieved from staging", {
  expect_equal(loadStagingData(registryName, dataName), d)
})

test_that("loading none-existing data returns false", {
  expect_false(loadStagingData(registryName, "imaginaryDataSet"))
})

test_that("deleting a none-existing file returns FALSE", {
  expect_false(deleteStagingData(registryName, "imaginaryDataSet"))
})

test_that("a file can be deleted", {
  expect_true(deleteStagingData(registryName, dataName))
  expect_false(file.exists(testFile))
})

test_that("a global clean of staging data can be performed (also dry run)", {
  expect_equal(saveStagingData(registryName, dataName, d), d)
  expect_true(file.exists(testFile))
  expect_message(cleanStagingData(0))
  expect_equal(class(cleanStagingData(0)), "character")
  expect_true(file.exists(testFile))
  expect_invisible(cleanStagingData(0, dryRun = FALSE))
  expect_false(file.exists(testFile))
})

# clean up config for file backend
unlink(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml"))

test_that("a global clean of staging data will stop if no parent directory", {
  Sys.unsetenv("R_RAP_CONFIG_PATH")
  expect_error(cleanStagingData(0))
})

# Test with db as backend
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())

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

test_that("env vars needed for db testing is present", {
  checkDb()
  expect_true("DB_HOST" %in% names(Sys.getenv()))
  expect_true("DB_USER" %in% names(Sys.getenv()))
  expect_true("DB_PASS" %in% names(Sys.getenv()))
})

test_config <- paste0(
  "r:",
  "\n  staging: ",
  "\n    target: db",
  "\n    key: staging\n"
)
cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml"))
writeLines(test_config, cf)
close(cf)

# make proper dbConfig
test_config <- paste0(
  registryName, ":",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : test_staging",
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : registryEphemaralUnitTesting\n",
  "staging:",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : test_staging",
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : dbBackendEphemaralUnitTesting\n"
)
cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)

test_that("No connection provided when no key (or connection object) given", {
  expect_error(dbStagingConnection(key = NULL, con = NULL))
})

test_that("No connection provided when insufficient config", {
  checkDb()
  expect_error(dbStagingConnection("unknown"), regexp = "Could not connect")
})

# make new staging database using prereq function
test_that("prereq creates database initially", {
  checkDb()
  expect_silent(dbStagingPrereq("staging"))
})

test_that("Error is returned when key cannot be found in config", {
  expect_error(dbStagingData("wrongEntry"))
})

test_that("A db connection object can be opened and closed", {
  checkDb()
  con <- dbStagingConnection(key = "staging")
  expect_true(inherits(con, "DBIConnection"))
  con <- dbStagingConnection(con = con)
  expect_true(is.null(con))
})

test_that("Data can be staged with db backend", {
  checkDb()
  d0 <- saveStagingData(registryName, "testData", d)
  expect_true(identical(d, d0))
})

test_that("staging files can be listed from db backend", {
  checkDb()
  v <- listStagingData(registryName)
  expect_equal(class(v), "character")
  expect_identical(v, "testData")
})

test_that("modification time of stagin data in db can be obtained", {
  checkDb()
  expect_true("POSIXct" %in% class(mtimeStagingData(registryName)))
})

test_that("retrieval of none existing data returns FALSE", {
  checkDb()
  expect_false(loadStagingData(registryName, "noSuchDataSet"))
})

test_that("data can be retrieved from staging db", {
  checkDb()
  expect_equal(loadStagingData(registryName, dataName), d)
})

test_that("deleting a none-existing dataset from db returns FALSE", {
  checkDb()
  expect_false(deleteStagingData(registryName, "imaginaryDataSet"))
})

test_that("a dataset can be deleted from db", {
  checkDb()
  expect_true(deleteStagingData(registryName, dataName))
  expect_false(loadStagingData(registryName, dataName))
})

test_that("a global clean of db staging data can be performed (also dry run)", {
  checkDb()
  expect_equal(saveStagingData(registryName, dataName, d), d)
  expect_identical(listStagingData(registryName), dataName)
  expect_message(cleanStagingData(0))
  expect_equal(class(cleanStagingData(0)), "character")
  expect_identical(listStagingData(registryName), dataName)
  expect_invisible(cleanStagingData(0, dryRun = FALSE))
  expect_false(loadStagingData(registryName, dataName))
})

if (is.null(checkDb(is_test_that = FALSE))) {
  dbStagingData("staging", drop = TRUE)
}

# Restore environment
unlink(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml"))
unlink(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
