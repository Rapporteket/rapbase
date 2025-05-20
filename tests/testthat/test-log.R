# store current instance and prepare
currentInstance <- Sys.getenv("R_RAP_INSTANCE")
currentConfig <- Sys.getenv("R_RAP_CONFIG_PATH")

tempdir <- tempdir()

file.copy(system.file("rapbaseConfig.yml", package = "rapbase"), tempdir)
configFile <- file.path(tempdir, "rapbaseConfig.yml")
config <- yaml::read_yaml(configFile)

Sys.setenv(R_RAP_CONFIG_PATH = tempdir)

# mocking when run as part of ci
Sys.setenv(R_RAP_INSTANCE = "DEV")

session <- list()
attr(session, "class") <- "ShinySession"

# must be last...
Sys.setenv(R_RAP_CONFIG_PATH = "")

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


# --- logging with database target ---
Sys.setenv(R_RAP_INSTANCE = currentInstance)

Sys.setenv(R_RAP_CONFIG_PATH = tempdir)

# adjust config and get whatever name of logging database define there
nameLogDb <- "raplogTest"
config$r$raplog$target <- "db"
config$r$raplog$key <- nameLogDb
yaml::write_yaml(config, configFile)

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
  expect_silent(appendLog(event = appEvent, name = "appLog"))
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

test_that("append and read errors when target is not known", {
  check_db()
  conf <- yaml::read_yaml(file.path(tempdir, "rapbaseConfig.yml"))
  conf$r$raplog$target <- "unknown"
  yaml::write_yaml(conf, file.path(tempdir, "rapbaseConfig.yml"))
  expect_error(appendLog(event = appEvent, name = "appLog"))
  expect_error(rapbase:::readLog(type = "app"))
  expect_error(rapbase:::sanitizeLog())
})

# remove test db
query_db(paste0("DROP DATABASE ", nameLogDb, ";"))

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfig)
Sys.setenv(R_RAP_INSTANCE = currentInstance)

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
