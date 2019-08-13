context("Github interaction")

test_that("How does code coverage work...", {
  expect_error(installGithubPackage())
})

test_that("Error is provided when no config present", {
  expect_error(installGithubPackage("norummy", "rel", FALSE))
})