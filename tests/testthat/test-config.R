context("Handle configuration")

test_that("Error is provided if configuration is not present in package", {
  expect_error(getConfig(fileName = "iDoNotExist.yml"))
})

test_that("Configuration is read from package", {
  expect_true(file.exists(system.file("dbConfig.yml", package = "rapbase")))
  expect_output(str(getConfig()), "List of")
})

test_that("Configuration can be read from environment var", {
  sample_config = list(first = list(a=1, b=2), second = list(a=3, b=4))
  sample_config_file = "test.yml"
  Sys.setenv(R_RAP_CONFIG_PATH = "~")
  writeLines(yaml::as.yaml(sample_config), file.path("~", sample_config_file))
  expect_output(str(getConfig(fileName = sample_config_file)), "List of")
})

test_that("Error is provided of configuration is nor present in file system", {
  sample_config_file = "iDoNotExist.yml"
  Sys.setenv(R_RAP_CONFIG_PATH = "~")
  expect_error(getConfig(fileName = sample_config_file))
})