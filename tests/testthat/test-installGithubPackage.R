context("Github interaction")

test_that("How does code coverage work...", {
  expect_error(installGithubPackage())
})

test_tnat("Error is provided when no config present", {
  expect_error(installGithubPackage("norummy", "rel", TRUE))
})

test_that("Error is provided when none existing branch in 'rapbase'", {
  expect_error(installGithubPackage("rapbase", "noneExistingBranch"))
})

test_that("Error provided when none existing branch for existing package", {
  expect_error(installGithubPackage("nordummy", "noneExistingBranch"))
})

test_that("Error provided wheb noen existing package", {
  expect_error(installGithubPackage("noneExistingPackage", "rel"))
})

test_that("Test package 'nordummy' can be installed from github", {
  expect_message(installGithubPackage("nordumm", "rel"))
})

test_that("'rapbase' can be instlled from github", {
  expect_message(installGithubPackage("rapbase", "rel"))
})