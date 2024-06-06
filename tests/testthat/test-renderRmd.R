# Somehow, latex compiling does not work on win ci. Since I currently cannot
# spend time debugging this skip these test on win based on same criteria as
# db test
checkTinytex <- function() {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    testthat::skip("Test skipped Windblows. Please fix later!")
  }
}

test_that("Rmd source can be rendered", {
  checkTinytex()
  sourceFile <- system.file("testReportSource.Rmd", package = "rapbase")
  logoFile <- system.file("template/logo.png", package = "rapbase")
  expect_true(file.exists(renderRmd(sourceFile)))
  expect_true("html" %in%
    class(renderRmd(sourceFile, outputType = "html_fragment")))
  expect_true(file.exists(renderRmd(sourceFile,
    outputType = "pdf",
    logoFile = logoFile,
    params = list(reglogo = "logo")
  )))
  expect_error(renderRmd(sourceFile, outputType = "beamer"))
  expect_error(renderRmd(sourceFile = "noneExistingFile.Rmd"))
})
