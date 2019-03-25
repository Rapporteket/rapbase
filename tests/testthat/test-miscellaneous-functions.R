context("Miscellaneos functions")

test_that("Listing of Rapporteket packages is present", {
  expect_true(is.vector(getRapPackages()))
})
