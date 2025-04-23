context("Auto report")

# store current instance
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")
Sys.setenv(R_RAP_CONFIG_PATH = "")

# make test data
synopsis <- "Test of auto report"
package <- "rapbase"
type <- "subscription"
fun <- ".testAutoReport"
paramNames <- c("aNum", "aChar", "anExp")
paramValues <- c(1, "someString", "Sys.Date()")
owner <- "tester"
ownerName <- "Tore Tester"
email <- "tester@skde.no"
organization <- "000000"
runDayOfYear <- as.POSIXlt(Sys.Date())$yday + 1
dryRun <- FALSE

test_that("error is provided when filtering on unknown entity", {
  expect_error(filterAutoRep(list(), "unknown_entity", c("test")))
})

test_that("auto reports can be filterd by valid entity on empty input", {
  expect_true(is.list(filterAutoRep(list(), "package", c("test"))))
})

test_that("Auto report can be created as dry run (stout)", {
  res <- createAutoReport(synopsis, package, type, fun, paramNames,
    paramValues, owner, ownerName, email, organization,
    runDayOfYear,
    dryRun = TRUE
  )
  expect_true(is.list(res))
})

test_that("auto report can be created as dry run (stout) in an PROD context", {
  res <- createAutoReport(synopsis, package, type, fun, paramNames,
    paramValues, owner, ownerName, email, organization,
    runDayOfYear,
    dryRun = TRUE
  )
  expect_true(is.list(res))
})

test_that("Function for testing automated reports return a file", {
  expect_true(file.exists(.testAutoReport()))
})

test_that("A year-day sequence can be mande", {
  rdoy <- makeRunDayOfYearSequence(interval = "month")
  expect_gte(length(rdoy), 10)
  expect_true(is.numeric(rdoy))
})

######################################
# Test findNextRunDate with db-setup #
######################################

test_that("The next run day is a monday", {
  expect_true(
    findNextRunDate(
      startDate = "2025-01-06", # Monday
      terminateDate = Sys.Date() + 365,
      interval = "weeks",
      returnFormat = "%A"
    ) %in% c("Monday", "mandag"))
})

test_that("The next run day is day 6 of month ", {
  expect_equal(
    findNextRunDate(
      startDate = "2025-01-06",
      terminateDate = Sys.Date() + 365,
      interval = "months",
      returnFormat = "%d"
    )
    , "06")
})

test_that("Terminate date is before today, and will return terminateDate + 1", {
  expect_equal(
    findNextRunDate(
      startDate = "2023-01-06",
      terminateDate = Sys.Date() - 10,
      interval = "months"
    ), format(Sys.Date() - 9, format = "%A %e. %B %Y"))

  expect_true(findNextRunDate(
    startDate = "2025-01-06",
    terminateDate = "2025-01-06"
  ) %in% c("Tuesday  7. January 2025", "tirsdag  7. januar 2025"))
})

test_that("Terminate date is after today but before next in range, and will return terminateDate + 1", {
  expect_equal(
    findNextRunDate(
      startDate = seq(as.Date(Sys.Date()), length = 2, by = "-24 months")[2], # 24 months back in time
      terminateDate = Sys.Date() + 14,
      interval = "months"
    ), format(Sys.Date() + 15, format = "%A %e. %B %Y"))
})


test_that("Start date is after today, and will return start date", {
  expect_equal(
    findNextRunDate(
      startDate = Sys.Date() + 10,
      terminateDate = Sys.Date() + 20,
      interval = "months"
    ), format(Sys.Date() + 10, format = "%A %e. %B %Y"))
})

test_that("findNextRunDate throw errors", {
  expect_error(findNextRunDate())
  expect_error(findNextRunDate(target = "db"))
  expect_error(findNextRunDate(startDate = "2025-01-06", target = "db"))
  expect_error(findNextRunDate(terminateDate = "2025-01-06", target = "db"))
  # Wrong date format
  expect_error(findNextRunDate(
    startDate = "202501-06",
    terminateDate = "2025-01-06",
    interval = "days"
  ))
  expect_error(findNextRunDate(
    startDate = "2025-01-06",
    terminateDate = "243467853443-212",
    interval = "days"
  ))
  # missing interval
  expect_error(findNextRunDate(
    startDate = "2025-01-06",
    terminateDate = Sys.Date() + 10
  ))
})


shinySession <- list(user = "tester")
shinySession$groups <- "rapbase"
attr(shinySession, "class") <- "ShinySession"

mapOrgId <- data.frame(id = "999999", name = "HUS", stringsAsFactors = FALSE)


test_that("Writing conf with undefined R_RAP_CONFIG_PATH provides an error", {
  expect_error(writeAutoReportData(config = NULL))
})

Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
file.copy(
  system.file("rapbaseConfig.yml", package = "rapbase"),
  Sys.getenv("R_RAP_CONFIG_PATH"),
  overwrite = TRUE
)


# Restore environment
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
