context("Miscellaneos functions")

test_that("Listing of Rapporteket packages is present", {
  expect_true(is.vector(getRapPackages()))
})

test_that("Widget for shiny apps can be provided", {
  expect_output(str(appNavbarUserWidget()), "script|navbar-brand")
})

# currently, testing of scheduler function is not at all sufficient...
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")
currentContext <- Sys.getenv("R_RAP_INSTANCE")
tempdir <- tempdir()
Sys.setenv(R_RAP_CONFIG_PATH=tempdir)
Sys.setenv(R_RAP_INSTANCE="")
file.copy(system.file(c("rapbaseConfig.yml", "dbConfig.yml"),
                      package = "rapbase"),
          tempdir)
test_that("scheduler kick-off function can be run based on default config (that should provide warnings)", {
  expect_warning(fireInTheHole())
})

# ...and the only testable artifact is the logArchive directory
test_that("fireInTheHole() left a logArchice directory", {
  expect_true(dir.exists(file.path(tempdir, "logArchive")))
})

# Restore env
Sys.setenv(R_RAP_INSTANCE=currentContext)
Sys.setenv(R_RAP_CONFIG_PATH=currentConfigPath)
