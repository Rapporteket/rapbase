context("Auto report")

# store current instance
currentInstance <- Sys.getenv("R_RAP_INSTANCE")
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")
Sys.setenv(R_RAP_INSTANCE="")
Sys.setenv(R_RAP_CONFIG_PATH="")

synopsis <- "Test of auto report"
package <- "rapbase"
fun <- ".testAutoReport"
paramNames <- c("aNum", "aChar", "anExp")
paramValues <- c(1, "someString", "Sys.Date()")
owner <- "tester"
email <- "tester@skde.no"
runDayOfYear <- as.POSIXlt(Sys.Date())$yday+1
dryRun <- FALSE

test_that("Auto report can be created and written to file", {
  expect_silent(createAutoReport(synopsis, package, fun, paramNames,
                                 paramValues, owner, email, runDayOfYear,
                                 dryRun))
    
  })

test_that("Auto report can be created as dry run (stout)", {
  res <- createAutoReport(synopsis, package, fun, paramNames,
                          paramValues, owner, email, runDayOfYear,
                          dryRun = TRUE)
  expect_true(is.list(res))
})

rd <- readAutoReportData()

test_that("Auto reports can be filtered by registry/package", {
  expect_true(is.list(selectByReg(rd, package)))
})

test_that("Auto reports can be filtered by owner", {
  expect_true(is.list(selectByOwner(rd, owner)))
})

test_that("Registries/packages can be extracted from config", {
  expect_true(is.vector(getRegs(rd)))
})

test_that("Function for testing automated reports return a file", {
  expect_true(file.exists(.testAutoReport()))
})

test_that("Auto reports can be processed (shipment by email not tested)", {
  expect_message(runAutoReport(dryRun = TRUE), "No emails sent. Attachment is",
                 all = FALSE)
})

test_that("A year-day sequence can be mande", {
  rdoy <- makeRunDayOfYearSequence(interval = "month")
  expect_gte(length(rdoy), 10)
  expect_true(is.numeric(rdoy))
})

test_that("The next run day in sequence can be identified", {
  expect_equal(as.numeric(
    findNextRunDate(runDayOfYear = c(10,20,30), baseDayNum = 11,
                    returnFormat = "%j")), 20
    )
})

shinySession <- list(user="tester")
shinySession$groups <- "rapbase"
# shinySession$request <- list(HTTP_RESH_ID="789012")
# shinySession$request$HTTP_ROLE <- "LC"
# # make a copy for testing wrong class
# shinySessionWrongClass <- shinySession
# simulate ShinySession class for above list
attr(shinySession, "class") <- "ShinySession"
test_that("A per-user subscription table (for shiny) can be made", {
  expect_true(is.list(makeUserSubscriptionTab(session = shinySession)))
})

reportId <- names(rd)[length(rd)]

test_that("Auto report can be deleted", {
  expect_silent(deleteAutoReport(reportId))
  expect_true(is.na(names(readAutoReportData())[reportId]))
})


# Restore environment
Sys.setenv(R_RAP_INSTANCE=currentInstance)
Sys.setenv(R_RAP_CONFIG_PATH=currentConfigPath)
