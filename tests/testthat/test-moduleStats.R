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

currentLogKey <- Sys.getenv("MYSQL_DB_LOG")
dbLogKey <- "raplogTest"
Sys.setenv(MYSQL_DB_LOG = dbLogKey)

# Create log db
if (is.null(check_db(is_test_that = FALSE))) {
  create_log_db(dbLogKey)
  # Add log table to db
  con <- connect_db(dbname = dbLogKey)
  DBI::dbWriteTable(
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
      session$setInputs(type = "app")
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
Sys.setenv(MYSQL_DB_LOG = currentLogKey)
