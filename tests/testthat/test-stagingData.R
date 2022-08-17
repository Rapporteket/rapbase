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

test_that("staging cannot commence if paret directory does not exist", {
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

test_that("deleting a none-existing file returns FALE", {
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

test_that("a global clean of staging data will stop if no parent directory", {
  Sys.unsetenv("R_RAP_CONFIG_PATH")
  expect_error(cleanStagingData(0))
})

# Restore environment
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
