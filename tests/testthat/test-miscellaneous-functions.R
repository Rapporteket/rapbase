context("Miscellaneos functions")


test_that("Listing of Rapporteket packages is present", {
  expect_true(is.vector(getRapPackages()))
})

test_that("Widget for shiny apps can be provided", {
  expect_output(str(appNavbarUserWidget()), "script|navbar-brand")
})

test_that("widget for shiny apps can be provided with user info added", {
  expect_output(str(appNavbarUserWidget(addUserInfo = TRUE)), "script|navbar-brand")
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


# produce a pop-up text...
test_that("an html doc regarding 'how we deal with...' can be provided", {
  expect_output(howWeDealWithPersonalData(session = list()), "")
})

# a logical providing info if context is Rapporteket
test_that("we currently do not reside within a Rapportekte context", {
  expect_false(isRapContext())
})
Sys.setenv(R_RAP_INSTANCE="DEV")
test_that("we are now within a Rapportekte context", {
  expect_true(isRapContext())
})


# Restore env
Sys.setenv(R_RAP_INSTANCE=currentContext)
Sys.setenv(R_RAP_CONFIG_PATH=currentConfigPath)
