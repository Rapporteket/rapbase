# store current instance and prepare
currentConfig <- Sys.getenv("R_RAP_CONFIG_PATH")
Sys.setenv(R_RAP_CONFIG_PATH = "")
tempdir <- tempdir()

test_that("error is provided when config path env var is empty", {
  expect_error(raplogManager())
})

Sys.setenv(R_RAP_CONFIG_PATH = tempdir)
file.copy(system.file("rapbaseConfig.yml", package = "rapbase"), tempdir)
conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
archiveDir <- conf$r$raplog$archiveDir

raplogManager()

test_that("for now, just that archiveDir is created", {
  expect_true(dir.exists(file.path(tempdir, archiveDir)))
})

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfig)
