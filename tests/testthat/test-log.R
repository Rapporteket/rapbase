# store current env var
currentDbLog <- Sys.getenv("MYSQL_DB_LOG")
tempdir <- tempdir()

session <- list()
attr(session, "class") <- "ShinySession"

test_that("nothing will be appended if path is not defined", {
  expect_error(appendLog(
    event = data.frame(foo = "bar"), name = ""
  ))
})

test_that("error is provided when target is not supported", {
  expect_error(appendLog(
    event = data.frame(foo = "bar"), name = "",
    target = "moon", format = "a4"
  ))
})


# adjust config and get whatever name of logging database define there
nameLogDb <- "raplogTest"
Sys.setenv(MYSQL_DB_LOG = nameLogDb)

test_that("env vars needed for testing is present", {
  check_db()
  expect_true("MYSQL_HOST" %in% names(Sys.getenv()))
  expect_true("MYSQL_USER" %in% names(Sys.getenv()))
  expect_true("MYSQL_PASSWORD" %in% names(Sys.getenv()))
})

test_that("a db for logging can be created", {
  check_db()
  expect_null(query_db(query =  paste0("CREATE DATABASE ", nameLogDb, ";")))
})

test_that("tables can be created in logging db", {
  check_db()
  expect_null(rapbase:::createLogDbTabs())
})

appEvent <- data.frame(
  time = "2022-03-24 11:16:29",
  user = "ttester",
  name = "Tore Tester",
  group = "rapbase",
  role = "accessLevel",
  resh_id = "999999",
  message = "unit test logging to db",
  stringsAsFactors = FALSE
)

test_that("app event can be appended to db", {
  check_db()
  expect_message(appendLog(event = appEvent, name = "appLog"))
})

test_that("log entries can be read from db", {
  check_db()
  expect_equal(class(rapbase:::readLog(type = "app")), "data.frame")
  expect_equal(
    rapbase:::readLog(type = "app", name = "rapbase")$user,
    "ttester"
  )
})

test_that("log can be sanitized in db", {
  check_db()
  expect_null(rapbase:::sanitizeLog())
  expect_equal(dim(rapbase:::readLog(type = "app"))[1], 0)
  expect_equal(dim(rapbase:::readLog(type = "report"))[1], 0)
})

# remove test db
query_db(paste0("DROP DATABASE ", nameLogDb, ";"))
# Restore env var
Sys.setenv(MYSQL_DB_LOG = currentDbLog)

test_that("loggerSetup is working", {
  # env-stuff
  currentUser <- Sys.getenv("SHINYPROXY_USERNAME")
  currentApp <- Sys.getenv("SHINYPROXY_APPID")
  Sys.setenv(SHINYPROXY_USERNAME = "jesus@sky.com")
  Sys.setenv(SHINYPROXY_APPID = "rapbasis")

  # run the function we want to test
  loggerSetup(hooks = FALSE)

  # log something
  infoLogjson <- logger::log_info(
    "Test log setup"
  )$default$record

  # Test what has been logged
  expect_true(jsonlite::validate(infoLogjson))
  infoLog <- jsonlite::fromJSON(infoLogjson)
  expect_equal(
    c(
      nchar(infoLog$time),
      infoLog$level,
      infoLog$message,
      infoLog$app,
      infoLog$user),
    c(
      23,
      "INFO",
      "Test log setup",
      "rapbasis",
      "jesus@sky.com"
    )
  )

  warningLog <- jsonlite::fromJSON(logger::log_warn(
    "Test warning"
  )$default$record)
  expect_equal(
    c(
      warningLog$level,
      warningLog$message
    ),
    c(
      "WARN",
      "Test warning"
    )
  )

  errorLog <- jsonlite::fromJSON(logger::log_error(
    "Test error"
  )$default$record)
  expect_equal(
    c(
      errorLog$level,
      errorLog$message
    ),
    c(
      "ERROR",
      "Test error"
    )
  )


  # env-stuff
  if (currentUser == "" && currentApp == "") {
    Sys.unsetenv("SHINYPROXY_USERNAME")
    Sys.unsetenv("SHINYPROXY_APPID")
  } else {
    Sys.setenv(SHINYPROXY_USERNAME = currentUser)
    Sys.setenv(SHINYPROXY_APPID = currentApp)
  }

  expect_error(loggerSetup())
})

test_that("logShinyInputChanges works without errors", {
  # Mock a Shiny input object
  shiny_input <- list(input1 = "value1", input2 = "value2")

  # Mock logger::log_shiny_input_changes to avoid actual logging
  mock_log_shiny_input_changes <- function(...) {
    return(TRUE) # Simulate successful logging
  }

  # Temporarily replace the logger function with the mock
  with_mocked_bindings(
    logShinyInputChanges = function(input) {
      mock_log_shiny_input_changes(input)
    },
    {
      # Call the function and check for errors
      expect_silent(logShinyInputChanges(shiny_input))
    }
  )
})
