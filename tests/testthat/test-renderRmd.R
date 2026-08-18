# Somehow, latex compiling does not work on win ci. Since I currently cannot
# spend time debugging this skip these test on win based on same criteria as
# db test
checkTinytex <- function() {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    testthat::skip("Test skipped Windblows. Please fix later!")
  }
}

test_that("Arguments are validated", {
  sourceFile <- system.file("testReportSource.Rmd", package = "rapbase")
  expect_error(renderRmd("noneExistingFile.Rmd"), "sourceFile")
  expect_error(renderRmd(sourceFile, outputType = "beamer"), "outputType")
  expect_error(renderRmd(sourceFile, template = "bogus"), "template")
  expect_error(renderRmd(sourceFile, template = "NULL"), "template")
  expect_error(renderRmd(sourceFile, quiet = NA), "quiet")
})

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
  expect_true(file.exists(renderRmd(sourceFile,
    outputType = "pdf",
    template = NULL
  )))
})
