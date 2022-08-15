# store current instance and prepare
currentInstance <- Sys.getenv("R_RAP_INSTANCE")
currentConfig <- Sys.getenv("R_RAP_CONFIG_PATH")

# Database infrastructure is only guaranteed at Github Actions and our own
# dev env.
# Tests running on other environments should be skipped:
check_db <- function(is_test_that = TRUE) {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    if (is_test_that) {
      testthat::skip("Possible lack of database infrastructure")
    } else {
      1
    }
  }
}

tempdir <- tempdir()

file.copy(system.file("rapbaseConfig.yml", package = "rapbase"), tempdir)
configFile <- file.path(tempdir, "rapbaseConfig.yml")

# make sure we test with file as target
config <- yaml::read_yaml(configFile)
config$r$raplog$target <- "file"
yaml::write_yaml(config, configFile)

Sys.setenv(R_RAP_CONFIG_PATH = tempdir)

# mocking when run as part of ci
Sys.setenv(R_RAP_INSTANCE = "DEV")

session <- list()
attr(session, "class") <- "ShinySession"

# Log events to files

test_that("logging is performed at application level", {
  expect_silent(appLogger(session))
  expect_true(file.exists(file.path(tempdir, "appLog.csv")))
})

test_that("logging is performed at report level", {
  expect_silent(repLogger(session))
  expect_true(file.exists(file.path(tempdir, "reportLog.csv")))
})

test_that("logging can be made by (automated) reports outside session", {
  expect_silent(autLogger(
    user = "ttest", name = "Tore Tester",
    registryName = "stats", reshId = "999999",
    type = "bulletin",
    pkg = "testpkg",
    fun = "testfun",
    param = list(testparam = "test")
  ))
})

test_that("formatter returns error upon non-existing format", {
  expect_error(makeLogRecord(content = list(), format = "a4"))
})

test_that("formatter returns a data.frame-class object", {
  expect_equal(class(makeLogRecord(content = list())), "data.frame")
})

test_that("formatter returns as expected", {
  expect_equal(makeLogRecord(list(val = 0))$val, 0)
})

test_that("log entries can be read from file", {
  expect_error(rapbase:::readLog(type = "noneExistingLogType"))
  expect_equal(class(rapbase:::readLog(type = "report")), "data.frame")
  expect_equal(
    rapbase:::readLog(type = "report", name = "stats")$user,
    "ttest"
  )
})

config$r$raplog$eolDays <- -1L
yaml::write_yaml(config, configFile)
test_that("log can be sanitized in files", {
  expect_null(rapbase:::sanitizeLog())
  expect_equal(dim(rapbase:::readLog(type = "app"))[1], 0)
  expect_equal(dim(rapbase:::readLog(type = "report"))[1], 0)
})

test_that("existing backup files can be overwritten", {
  expect_silent(autLogger(
    user = "ttest", name = "Tore Tester",
    registryName = "stats", reshId = "999999",
    type = "bulletin",
    pkg = "testpkg",
    fun = "testfun",
    param = list(testparam = "test")
  ))
  expect_equal(dim(rapbase:::readLog(type = "report"))[1], 1)
  expect_null(rapbase:::sanitizeLog())
  expect_equal(dim(rapbase:::readLog(type = "report"))[1], 0)
})


# must be last...
Sys.setenv(R_RAP_CONFIG_PATH = "")

test_that("nothing will be appended if path is not defined", {
  expect_error(appendLog(
    event = data.frame(foo = "bar"), name = ""
  ))
})

test_that("nothing will be appended if (file) format is not recognized", {
  expect_error(appendLog(
    event = data.frame(foo = "bar"), name = "",
    target = "file", format = "a4"
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
config$r$raplog$target <- "db"
yaml::write_yaml(config, configFile)
nameLogDb <- "raplogTest"

test_that("env vars needed for testing is present", {
  check_db()
  expect_true("DB_HOST" %in% names(Sys.getenv()))
  expect_true("DB_USER" %in% names(Sys.getenv()))
  expect_true("DB_PASS" %in% names(Sys.getenv()))
})

# make temporary config
test_config <- paste0(
  config$r$raplog$key, ":",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : ", nameLogDb,
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : ephemaralUnitTesting\n"
)

cf <- file(file.path(tempdir, "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)

test_that("a db for logging can be created", {
  check_db()
  expect_true(rapbase:::createLogDb(nameLogDb))
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
})

# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    bigint = "integer"
  )
  RMariaDB::dbExecute(con, paste("DROP DATABASE", nameLogDb))
  rapbase::rapCloseDbConnection(con)
}

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfig)
Sys.setenv(R_RAP_INSTANCE = currentInstance)
