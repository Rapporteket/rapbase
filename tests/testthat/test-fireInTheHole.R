currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")
currentContext <- Sys.getenv("R_RAP_INSTANCE")
tempdir <- tempdir()
Sys.setenv(R_RAP_CONFIG_PATH = tempdir)
Sys.setenv(R_RAP_INSTANCE = "")
file.copy(
  system.file(c("rapbaseConfig.yml", "dbConfig.yml", "autoReport.yml"),
    package = "rapbase"
  ),
  tempdir
)

# We do not (yet) have testing facilities to properly test email shipment.
# The strategy here is therefore to force the built-in test auto reports
# to run only on the none-existing day number 0 to make sure
# no reports are scheduled for shipment on the day of testing (any day)

reps <- readAutoReportData()
reps$testAutoReportFirst$runDayOfYear <- c(0)
reps$testAutoReportSecond$runDayOfYear <- c(0)
reps$testAutoReportThird$runDayOfYear <- c(0)
writeAutoReportData(fileName = "autoReport.yml", reps)

test_that(paste(
  "scheduler kick-off function can be run based on default",
  "config (that should provide warnings)"
), {
  expect_silent(fireInTheHole())
  expect_silent(fireInTheHole(flipPeriod = TRUE))
})

# Restore env
Sys.setenv(R_RAP_INSTANCE = currentContext)
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
