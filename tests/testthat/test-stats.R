## store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

registryName <- "rapbase"

Sys.unsetenv("R_RAP_CONFIG_PATH")
test_that("error reading log is given when config path is not set", {
  expect_error(getRegistryLog(registryName))
})

# make pristine config path to avoid clutter from other tests
Sys.setenv(R_RAP_CONFIG_PATH = file.path(tempdir(), "statsTesting"))
dir.create(Sys.getenv("R_RAP_CONFIG_PATH"))
test_that("error reading log when the file does not exist", {
  expect_error(getRegistryLog(registryName))
})

write.table(
  rapbase::appLog,
  file = file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "appLog.csv"),
  append = FALSE, col.names = TRUE, sep = ","
)


# helper functions
test_that("raw log data are returned as data frame", {
  expect_equal(class(getRegistryLog(registryName)), "data.frame")
})
test_that("mutated log data are returned as data frame", {
  expect_equal(class(logFormat(getRegistryLog(registryName))), "data.frame")
})
test_that("time framed log data are returned as data frame", {
  expect_equal(
    class(
      logTimeFrame(logFormat(getRegistryLog(registryName)), Sys.Date(),
                   Sys.Date())
    ),
    "data.frame"
  )
})


# The remainig test the shiny modules
test_that("stats UC input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(statsInput("id")))
})

test_that("module server provides sensible output", {
  shiny::testServer(statsServer, args = list(registryName = registryName), {
    session$setInputs(type = "app")
    expect_equal(class(output$period), "list")
    session$setInputs(period = rep(Sys.time(), 2), downloadFormat = "csv")
    expect_equal(class(output$download), "character")
    session$setInputs(downloadFormat = "xlsx-csv")
    expect_true(file.exists(output$download))
  })
})

test_that("test app returns an app object", {
  expect_equal(class(statsApp()), "shiny.appobj")
})

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
