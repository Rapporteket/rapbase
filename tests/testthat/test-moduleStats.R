## store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

registryName <- "rapbase"

## db
test_that("env vars needed for testing is present", {
  check_db()
  expect_true("MYSQL_HOST" %in% names(Sys.getenv()))
  expect_true("MYSQL_USER" %in% names(Sys.getenv()))
  expect_true("MYSQL_PASSWORD" %in% names(Sys.getenv()))
})

# make pristine config path to avoid clutter from other tests
Sys.setenv(R_RAP_CONFIG_PATH = file.path(tempdir(), "statsTesting"))
dir.create(Sys.getenv("R_RAP_CONFIG_PATH"))

# some systems do not provide a database back-end, test only on file log target
confFile <- file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml")
file.copy(system.file("rapbaseConfig.yml", package = "rapbase"), confFile)
con <- file(confFile, "r")
conf <- yaml::read_yaml(con)
close(con)

currentLogKey <- Sys.getenv("MYSQL_DB_LOG")
dbLogKey <- conf$r$raplog$key
Sys.setenv(MYSQL_DB_LOG = dbLogKey)

# Create log db
if (is.null(check_db(is_test_that = FALSE))) {
  create_log_db(dbLogKey)
  # Add log table to db
  con <- connect_db(dbname = dbLogKey)
  RMariaDB::dbWriteTable(
    conn = con,
    name = "applog",
    value = rapbase::appLog, append = TRUE, header = TRUE, row.names = FALSE
  )
  close_db_connection(con)
  con <- NULL
}


# helper functions
test_that("mutated log data are returned as data frame", {
  check_db()
  expect_equal(
    class(logFormat(rapbase:::readLog(type = "app", name = registryName))),
    "data.frame"
  )
})
test_that("time framed log data are returned as data frame", {
  check_db()
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
  check_db()
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

# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  query <- c(
    paste0("DROP DATABASE IF EXISTS ", dbLogKey, ";")
  )
  query_db(query = query)
}

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
Sys.setenv(MYSQL_DB_LOG = currentLogKey)
