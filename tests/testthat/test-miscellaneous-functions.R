context("Miscellaneos functions")

test_that("Listing of Rapporteket packages is present", {
  expect_true(is.vector(getRapPackages()))
})

test_that("Widget for shiny apps can be provided", {
  expect_output(str(appNavbarUserWidget()), "script|navbar-brand")
})
