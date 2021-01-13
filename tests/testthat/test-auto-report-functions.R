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
email <- "tester@skde.no"
organization <- "000000"
runDayOfYear <- as.POSIXlt(Sys.Date())$yday + 1
dryRun <- FALSE

test_that("auto report config can be upgraded", {
  expect_message(upgradeAutoReportData(list(list(synopsis = "test"))))
})

test_that("already upgraded auto report config is left as is", {
  c <- list(list(type = "subscription"))
  expect_equal(c, upgradeAutoReportData(c))
})

test_that("Config data can be filterd by registry on empty input", {
  expect_true(is.list(selectByReg(list(), "test")))
})

test_that("config data can be filterd by type on empty input", {
  expect_true(is.list(selectByType(list(), "test")))
})

test_that("Config data can be filterd by owner on empty input", {
  expect_true(is.list(selectByOwner(list(), "test")))
})

test_that("Config data can be filtered by organization on empty input", {
  expect_true(is.list(selectByOrganization(list(), "test")))
})


test_that("Auto report can be created as dry run (stout)", {
  res <- createAutoReport(synopsis, package, type, fun, paramNames,
                          paramValues, owner, email, organization,
                          runDayOfYear, dryRun = TRUE)
  expect_true(is.list(res))
})

Sys.setenv(R_RAP_INSTANCE = "PRODUCTION")
test_that("auto report can be created as dry run (stout) in an PROD context", {
  res <- createAutoReport(synopsis, package, type, fun, paramNames,
                          paramValues, owner, email, organization,
                          runDayOfYear, dryRun = TRUE)
  expect_true(is.list(res))
})
Sys.setenv(R_RAP_INSTANCE = "")

rd <- readAutoReportData()

test_that("Auto reports can be filtered by registry/package", {
  expect_true(is.list(selectByReg(rd, package)))
})

test_that("Auto reports can be filtered by type", {
  expect_true(is.list(selectByType(rd, type)))
})

test_that("Auto reports can be filtered by owner", {
  expect_true(is.list(selectByOwner(rd, owner)))
})

test_that("Auto reports can be filtered by organization", {
  expect_true(is.list(selectByOrganization(rd, organization)))
})

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

test_that("The next run day in sequence can be identified", {
  expect_equal(as.numeric(
    findNextRunDate(runDayOfYear = c(10, 20, 30), baseDayNum = 11,
                    returnFormat = "%j")), 20
    )
})

shinySession <- list(user = "tester")
shinySession$groups <- "rapbase"
attr(shinySession, "class") <- "ShinySession"

mapOrgId <- data.frame(id='999999', name='HUS', stringsAsFactors = FALSE)

test_that("auto report tables (for shiny) can be made", {
  expect_true(is.list(
    makeAutoReportTab(shinySession, type = "subscription", mapOrgId = mapOrgId)
  ))
  expect_true(is.list(
    makeAutoReportTab(shinySession, type = "dispatchment", mapOrgId = mapOrgId)
  ))
  expect_true(is.list(
    makeAutoReportTab(shinySession, type = "bulletin", mapOrgId = mapOrgId)
  ))
})

test_that("a registry dispatchment table (for shiny) can be made", {
  expect_true(is.list(
    makeRegDispatchmentTab(shinySession, mapOrgId)
  ))
})

test_that("A per-user subscription table (for shiny) can be made", {
  expect_true(is.list(makeUserSubscriptionTab(session = shinySession)))
})

test_that("per user subscription table provides warning for deprecated v2", {
  expect_warning(makeUserSubscriptionTab_v2(session = shinySession))
})

test_that("per-user subscription table for v2 also provides dep warning", {
  expect_warning(makeUserSubscriptionTabV2(session = shinySession))
})

test_that("Writing conf with undefined R_RAP_CONFIG_PATH provides an error", {
  expect_error(writeAutoReportData(config = NULL))
  })

Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
file.copy(system.file("rapbaseConfig.yml", package = "rapbase"),
          Sys.getenv("R_RAP_CONFIG_PATH"))

test_that("Auto report config can be created from package default", {
  expect_warning(readAutoReportData())
})

# For a valid test make sure there is ONE standard dummy report scheduled for
# day 90
test_that("Auto reports can be processed (shipment by email not tested)", {
  expect_message(runAutoReport(dayNumber = 90, dryRun = TRUE),
                 "No emails sent. Attachment is", all = FALSE)
})

reportId <- names(rd)[length(rd)]

test_that("Auto report can be deleted", {
  expect_message(deleteAutoReport(reportId))
  expect_true(is.na(names(readAutoReportData())[reportId]))
})

test_that("Auto report can be created and written to file", {
  expect_silent(createAutoReport(synopsis, package, type, fun, paramNames,
                                 paramValues, owner, email, organization,
                                 runDayOfYear, dryRun))
})


test_that("Backup of auto report config can be made", {
  writeAutoReportData(config = rd)
  expect_true(file.exists(file.path(Sys.getenv("R_RAP_CONFIG_PATH"),
                                    "autoReportBackup")))
})

bckFile <- list.files(file.path(Sys.getenv("R_RAP_CONFIG_PATH"),
                                "autoReportBackup"), full.names = TRUE)
Sys.setFileTime(bckFile, "2019-01-01")
writeAutoReportData(config = rd)

f <- file.remove(
  list.files(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "autoReportBackup"),
             full.names = TRUE))
f <- file.remove(
  file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "autoReportBackup"))
f <- file.remove(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "autoReport.yml"))
Sys.setenv(R_RAP_CONFIG_PATH = "")

# Restore environment
Sys.setenv(R_RAP_INSTANCE = currentInstance)
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
