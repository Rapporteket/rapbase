## store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

registryName <- "rapbase"

# make pristine config path to avoid clutter from other tests
Sys.setenv(R_RAP_CONFIG_PATH = file.path(tempdir(), "statsTesting"))
dir.create(Sys.getenv("R_RAP_CONFIG_PATH"))

# some systems do not provide a database back-end, test only on file log target
confFile <- file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml")
file.copy(system.file("rapbaseConfig.yml", package = "rapbase"), confFile)
con <- file(confFile, "r")
conf <- yaml::read_yaml(con)
close(con)
conf$r$raplog$target <- "file"
yaml::write_yaml(conf, confFile)

# use package data to populate an app log
write.table(
  rapbase::appLog,
  file = file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "appLog.csv"),
  append = FALSE, col.names = TRUE, sep = ","
)


# helper functions
test_that("mutated log data are returned as data frame", {
  expect_equal(
    class(logFormat(rapbase:::readLog(type = "app", name = registryName))),
    "data.frame"
  )
})
test_that("time framed log data are returned as data frame", {
  expect_equal(
    class(
      logTimeFrame(
        logFormat(
          rapbase:::readLog(type = "app", name = registryName)
        ),
        Sys.Date(),
        Sys.Date()
      )
    ),
    "data.frame"
  )
})


# The remaining test the shiny modules
test_that("stats UC input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(statsInput("id")))
})

test_that("module server provides sensible output", {
  shiny::testServer(statsServer, args = list(registryName = registryName), {
    session$setInputs(type = "app")
    expect_equal(class(output$period), "list")
    session$setInputs(
      period = rep(Sys.Date(), 2),
      downloadFormat = "csv"
    )
    expect_equal(class(output$download), "character")
    session$setInputs(downloadFormat = "xlsx-csv")
    expect_true(file.exists(output$download))
  })
})

test_that("module server is restricted when not eligible", {
  shiny::testServer(
    statsServer,
    args = list(registryName = registryName, eligible = FALSE),
    {
      expect_true(is.null(output$downloadButton))
    }
  )
})

test_that("test app returns an app object", {
  expect_equal(class(statsApp()), "shiny.appobj")
})

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
