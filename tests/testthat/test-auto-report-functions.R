context("Auto report")

# store current instance
currentInstance <- Sys.getenv("R_RAP_INSTANCE")
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")
Sys.setenv(R_RAP_INSTANCE = "")
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

test_that("auto report config can be upgraded", {
  expect_message(upgradeAutoReportData(list(list(synopsis = "test"))))
})

test_that("already upgraded auto report config is left as is", {
  c <- list(list(
    type = "subscription", ownerName = "Tore Tester",
    startDate = "2021-11-19"
  ))
  expect_equal(c, upgradeAutoReportData(c))
})

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

Sys.setenv(R_RAP_INSTANCE = "PRODUCTION")
test_that("auto report can be created as dry run (stout) in an PROD context", {
  res <- createAutoReport(synopsis, package, type, fun, paramNames,
    paramValues, owner, ownerName, email, organization,
    runDayOfYear,
    dryRun = TRUE
  )
  expect_true(is.list(res))
})
Sys.setenv(R_RAP_INSTANCE = "")

rd <- readAutoReportData()

test_that("Registries/packages can be extracted from config", {
  expect_true(is.vector(getRegs(rd)))
})

test_that("Function for testing automated reports return a file", {
  expect_true(file.exists(.testAutoReport()))
})

test_that("A year-day sequence can be mande", {
  rdoy <- makeRunDayOfYearSequence(interval = "month")
  expect_gte(length(rdoy), 10)
  expect_true(is.numeric(rdoy))
})

test_that("The next run day in simple sequence can be identified", {
  expect_equal(as.numeric(
    findNextRunDate(
      runDayOfYear = c(10, 20, 30), baseDayNum = 11,
      returnFormat = "%j"
    )
  ), 20)
})

test_that("The next run day in sequence can be identified when next year", {
  expect_equal(as.numeric(
    findNextRunDate(
      runDayOfYear = c(10, 20, 30), baseDayNum = 31,
      returnFormat = "%j"
    )
  ), 10)
})

test_that("for within year-break sequence, next is found among earlier days", {
  expect_equal(as.numeric(
    findNextRunDate(
      runDayOfYear = c(200, 300, 1, 100), baseDayNum = 10,
      returnFormat = "%j"
    )
  ), 100)
})

test_that("for within year-break sequence, next is found among later days", {
  expect_equal(as.numeric(
    findNextRunDate(
      runDayOfYear = c(200, 300, 1, 100), baseDayNum = 110,
      returnFormat = "%j"
    )
  ), 200)
})

test_that("a start date is enforced when given", {
  todayNum <- as.POSIXlt(Sys.Date())$yday + 1
  # sequence of 4 consecutive days from, but not including, today
  days <-
    as.POSIXlt(seq.Date(Sys.Date(), (Sys.Date() + 3), by = "day"))$yday + 2
  expect_equal(as.numeric(
    findNextRunDate(
      runDayOfYear = days, baseDayNum = todayNum,
      returnFormat = "%j"
    )
  ), days[1])
  startDate <- Sys.Date() + 4
  expect_equal(as.numeric(
    findNextRunDate(
      runDayOfYear = days, baseDayNum = todayNum, startDate = startDate,
      returnFormat = "%j"
    )
  ), days[4])
})

shinySession <- list(user = "tester")
shinySession$groups <- "rapbase"
attr(shinySession, "class") <- "ShinySession"

mapOrgId <- data.frame(id = "999999", name = "HUS", stringsAsFactors = FALSE)

test_that("auto report tables (for shiny) can be made", {
  expect_true(is.list(
    makeAutoReportTab(shinySession,
      type = "subscription", mapOrgId = mapOrgId,
      includeReportId = TRUE
    )
  ))
  expect_true(is.list(
    makeAutoReportTab(shinySession,
      type = "dispatchment", mapOrgId = mapOrgId,
      includeReportId = TRUE
    )
  ))
  expect_true(is.list(
    makeAutoReportTab(shinySession,
      type = "bulletin", mapOrgId = mapOrgId,
      includeReportId = TRUE
    )
  ))
})

test_that("a registry dispatchment table (for shiny) can be made", {
  expect_true(is.list(
    makeAutoReportTab(shinySession, type = "dispatchment", mapOrgId)
  ))
})

test_that("Writing conf with undefined R_RAP_CONFIG_PATH provides an error", {
  expect_error(writeAutoReportData(config = NULL))
})

Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
file.copy(
  system.file("rapbaseConfig.yml", package = "rapbase"),
  Sys.getenv("R_RAP_CONFIG_PATH"),
  overwrite = TRUE
)

test_that("Auto report config can be created from package default", {
  expect_warning(readAutoReportData())
})

# For a valid test make sure there is ONE standard dummy report scheduled for
# day 90
test_that("Auto reports can be processed (shipment by email not tested)", {
  expect_message(runAutoReport(dayNumber = 90, dryRun = TRUE),
    "No emails sent. Content is:",
    all = FALSE
  )
})

# Do the same for a bulletin, above conditions also apply!
test_that("Auto reports can be processed (shipment by email not tested)", {
  expect_message(
    runAutoReport(dayNumber = 90, type = c("bulletin"), dryRun = TRUE),
    "No emails sent. Content is: This is a simple",
    all = FALSE
  )
})

test_that("a report sceduled for today with startDate in future is not run", {
  # skip if today is one of built in report test days
  skip_if((as.POSIXlt(Sys.Date())$yday + 1) %in% c(30, 60, 90),
    message = "Today is one of three predefined test days."
  )
  createAutoReport(synopsis, package, type, fun, paramNames,
    paramValues, owner, ownerName, email, organization,
    runDayOfYear = as.numeric(format(Sys.Date(), "%j")),
    startDate = as.character(Sys.Date() + 1)
  )
  ## update rd to be used later
  rd <- readAutoReportData()
  expect_silent(
    runAutoReport(dryRun = TRUE)
  )
})

# Try to send an email, but expect error since there is no viable smtp set-up
test_that("Auto reports can be processed and emailed (but failing send)", {
  skip("cran autocheck false positive on debian")
  expect_warning(
    runAutoReport(dayNumber = 90, type = c("bulletin"), dryRun = FALSE)
  )
})

createAutoReport(synopsis, package, type, fun, paramNames,
  paramValues, owner, ownerName, email, organization,
  runDayOfYear = as.numeric(format(Sys.Date(), "%j")),
  startDate = as.character(Sys.Date() + 1)
)
reportId <- names(rd)[length(rd)]

test_that("Auto report can be deleted", {
  expect_silent(deleteAutoReport(reportId))
  expect_true(is.na(names(readAutoReportData())[reportId]))
})

test_that("Auto report can be created and written to file", {
  expect_silent(createAutoReport(
    synopsis, package, type, fun, paramNames,
    paramValues, owner, email, organization,
    runDayOfYear, dryRun
  ))
})


test_that("Backup of auto report config can be made", {
  writeAutoReportData(config = rd)
  expect_true(file.exists(file.path(
    Sys.getenv("R_RAP_CONFIG_PATH"),
    "autoReportBackup"
  )))
})

bckFile <- list.files(file.path(
  Sys.getenv("R_RAP_CONFIG_PATH"),
  "autoReportBackup"
), full.names = TRUE)
Sys.setFileTime(bckFile, "2019-01-01")
writeAutoReportData(config = rd)

f <- file.remove(
  list.files(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "autoReportBackup"),
    full.names = TRUE
  )
)
f <- file.remove(
  file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "autoReportBackup")
)
f <- file.remove(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "autoReport.yml"))
Sys.setenv(R_RAP_CONFIG_PATH = "")

# Restore environment
Sys.setenv(R_RAP_INSTANCE = currentInstance)
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
