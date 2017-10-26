context("Github interaction")

test_that("How does code coverage work...", {
  expect_error(installGithubPackage())
})

test_tnat("Error is provided when no config present", {
  expect_error(installGithubPackage("norummy", "rel", TRUE))
})

test_that("Test package 'nordummy' can be installed from github", {
  expect_message(installGithubPackage("nordumm", "rel"))
})

test_that("'rapbase' can be instlled from github", {
  expect_message(installGithubPackage("rapbase", "rel"))
})