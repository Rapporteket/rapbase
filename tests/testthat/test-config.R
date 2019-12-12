context("Handle configuration")

# store current instance
currentInstance <- Sys.getenv("R_RAP_INSTANCE")
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

test_that("Error is provided if configuration is not present in package", {
  expect_error(getConfig(fileName = "iDoNotExist.yml"))
})

test_that("Configuration is read from package", {
  expect_true(file.exists(system.file("dbConfig.yml", package = "rapbase")))
  expect_output(str(getConfig()), "List of")
})

sample_config_file = tempfile()
Sys.setenv(R_RAP_CONFIG_PATH = dirname(sample_config_file))

test_that("Configuration can be read from environment var", {
  sample_config = list(first = list(a=1, b=2), second = list(a=3, b=4))
  writeLines(yaml::as.yaml(sample_config), sample_config_file)
  expect_output(str(getConfig(fileName = basename(sample_config_file))), "List of")
})

test_that("Error is provided of configuration is nor present in file system", {
  sample_config_file = "iDoNotExist.yml"
  expect_error(getConfig(fileName = sample_config_file))
})

# Restore environment
Sys.setenv(R_RAP_INSTANCE=currentInstance)
Sys.setenv(R_RAP_CONFIG_PATH=currentConfigPath)