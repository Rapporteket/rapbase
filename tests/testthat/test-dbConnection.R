context("Handling db connections")

test_that("Error provided when key has no corresponding config", {
  expect_error(rapOpenDbConnection(registryName = "aNoneExistingRegistryKey"))
})