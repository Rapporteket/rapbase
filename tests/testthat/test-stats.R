## store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
write.table(
  rapbase::appLog,
  file = file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "appLog.csv"),
  append = FALSE, col.names = TRUE, sep = ","
)

registryName <- "rapbase"

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

test_that("guide module server provides sensible output", {
  shiny::testServer(statsServer, args = list(registryName = registryName), {
    session$setInputs(type = "app")
    expect_equal(class(output$period), "list")
    #session$setInputs(exportPid = "areedv")
    #expect_equal("character", class(pubkey()))
    #session$setInputs(exportKey = pubkey())
    #expect_equal(class(output$exportKeyUI), "list")
    #session$setInputs(exportCompress = TRUE)
    #expect_true(is.null(rv$exportFile))
    #expect_equal(class(output$exportEncryptUI), "list")
  })
})

test_that("guide test app returns an app object", {
  expect_equal(class(statsApp()), "shiny.appobj")
})

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
