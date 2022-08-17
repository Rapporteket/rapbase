context("Miscellaneos functions")


test_that("Listing of Rapporteket packages is present", {
  expect_true(is.vector(getRapPackages()))
})

test_that("Widget for shiny apps can be provided", {
  expect_output(str(appNavbarUserWidget()), "script|navbar-brand")
})

test_that("widget for shiny apps can be provided with user info added", {
  expect_output(
    str(appNavbarUserWidget(addUserInfo = TRUE)),
    "script|navbar-brand"
  )
})


currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")
currentContext <- Sys.getenv("R_RAP_INSTANCE")
tempdir <- tempdir()
Sys.setenv(R_RAP_CONFIG_PATH = tempdir)
Sys.setenv(R_RAP_INSTANCE = "")
file.copy(
  system.file(c("rapbaseConfig.yml", "dbConfig.yml"),
    package = "rapbase"
  ),
  tempdir
)


# produce a pop-up text...
test_that("an html doc regarding 'how we deal with...' can be provided", {
  expect_output(howWeDealWithPersonalData(session = list()), "")
})

test_that("pop-up html can add an installed pacakge to info", {
  expect_output(howWeDealWithPersonalData(
    session = list(),
    callerPkg = "base"
  ), "")
})

test_that("pop-up html provide a warning for a non-existing pacakge", {
  expect_warning(howWeDealWithPersonalData(
    sessio = list(),
    callerPkg = "notapackage"
  ))
})

# a logical providing info if context is Rapporteket
test_that("we currently do not reside within a Rapporteket context", {
  expect_false(isRapContext())
})
Sys.setenv(R_RAP_INSTANCE = "DEV")
test_that("we are now within a Rapporteket context", {
  expect_true(isRapContext())
})


# Restore env
Sys.setenv(R_RAP_INSTANCE = currentContext)
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
