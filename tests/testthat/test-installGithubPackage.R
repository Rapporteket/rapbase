context("Github interaction")

test_that("How does code coverage work...", {
  expect_error(installGithubPackage())
})

test_that("Error is provided when no config present", {
  expect_error(installGithubPackage("norummy", "rel", FALSE))
})

test_that("Not found (404) printed when none existing branch in 'rapbase'", {
  expect_output(print(installGithubPackage("rapbase", "noneExistingBranch")),
                "HTTP 404")
})

test_that("Not found (404) when none existing branch for existing package", {
  expect_output(print(installGithubPackage("nordummy", "noneExistingBranch")),
                "HTTP 404")
})

test_that("Not found (404) when none existing package", {
  expect_output(print(installGithubPackage("noneExistingPackage", "rel")),
                "HTTP 404")
})

test_that("Test package 'nordummy' can be installed from github", {
  expect_message(installGithubPackage("nordummy", "rel"))
})

test_that("'rapbase' can be instlled from github", {
  expect_message(installGithubPackage("rapbase", "rel"))
})