test_that("Rmd source can be rendered", {
	sourceFile <- system.file("testReportSource.Rmd", package = "rapbase")
	logoFile <- system.file("template/logo.png", package = "rapbase")
  expect_true(file.exists(renderRmd(sourceFile)))
  expect_true("html" %in%
  							class(renderRmd(sourceFile, outputType = "html_fragment")))
  expect_true(file.exists(renderRmd(sourceFile, outputType = "pdf",
  																	logoFile = logoFile,
  																	params = list(reglogo = "logo"))))
  expect_error(renderRmd(sourceFile, outputType = "beamer"))
  expect_error(renderRmd(sourceFile = "noneExistingFile.Rmd"))
})
