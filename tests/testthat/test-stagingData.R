# test data
registryName <- "testReg"
dir <- tempdir()
dataName <- "testData"
d <- mtcars
testPath <- file.path(dir, "stagingData", registryName)
testFile <- file.path(testPath, dataName)

test_that("staging cannot commence if paret directory does not exist", {
  expect_error(pathStagingData(registryName, dir = "imaginaryDir"))
  expect_error(
    saveStagingData(registryName, "testData", d, dir = "imaginaryDir")
  )
})

test_that("relevant directories are created at first time use", {
  expect_equal(pathStagingData(registryName, dir), testPath)
})

test_that("data can be stored for staging", {
  expect_equal(saveStagingData(registryName, dataName, d, dir), d)
  expect_true(file.exists(testFile))
})

test_that("data can be retrieved from staging", {
  expect_equal(loadStagingData(registryName, dataName, dir), d)
})

test_that("loading none-existing data returns false", {
  expect_false(loadStagingData(registryName, "imaginaryDataSet", dir))
})