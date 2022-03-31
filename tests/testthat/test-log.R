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
    registryName = "rapbase", reshId = "999999",
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


# logging with database target
Sys.setenv(R_RAP_CONFIG_PATH = tempdir)

test_that("env vars needed for testing is present", {
  check_db()
  expect_true("DB_HOST" %in% names(Sys.getenv()))
  expect_true("DB_USER" %in% names(Sys.getenv()))
  expect_true("DB_PASS" %in% names(Sys.getenv()))
})

# prep db for testing
if (is.null(check_db(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                             host = Sys.getenv("DB_HOST"),
                             user = Sys.getenv("DB_USER"),
                             password = Sys.getenv("DB_PASS"),
                             bigint = "integer"
  )
  RMariaDB::dbExecute(con, "CREATE DATABASE testRaplog;")
  RMariaDB::dbDisconnect(con)
}

# make temporary config
test_config <- paste0(
  "raplog:",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : testRaplog",
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : ephemaralUnitTesting\n"
)

#file.copy(system.file("dbConfig.yml", package = "rapbase"), tempdir)
cf <- file(file.path(tempdir, "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)

# make queries for creating tables
fc <- file(system.file("createRaplogTabs.sql", package = "rapbase"), "r")
t <- readLines(fc)
close(fc)
sql <- paste0(t, collapse = "\n")
queries <- strsplit(sql, ";")[[1]]

registry_name <- "raplog"

test_that("relevant test database and tables can be made", {
  check_db()
  con <- rapbase::rapOpenDbConnection(registry_name)$con
  for (i in seq_len(length(queries))) {
    expect_equal(class(RMariaDB::dbExecute(con, queries[i])), "integer")

  }
  rapbase::rapCloseDbConnection(con)
})

config$r$raplog$target <- "db"
yaml::write_yaml(config, configFile)
appEvent <- data.frame(
  time = "2022-03-24 11:16:29",
  user = "ttester",
  name = "Tore Tester",
  group = "rapbase",
  role = "accessLevel",
  resh_id = "999999",
  message = "unit test logging to db"
)

test_that("app event can be appended to db", {
  check_db()
  expect_warning(appendLog(event = appEvent, name = "appLog"))
})

test_that("append errors when target is not known", {
  check_db()
  conf <- yaml::read_yaml(file.path(tempdir, "rapbaseConfig.yml"))
  conf$r$raplog$target <- "unknown"
  yaml::write_yaml(conf, file.path(tempdir, "rapbaseConfig.yml"))
  expect_error(appendLog(event = appEvent, name = "appLog"))
})

# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  con <- rapbase::rapOpenDbConnection(registry_name)$con
  RMariaDB::dbExecute(con, "DROP DATABASE testRaplog;")
  rapbase::rapCloseDbConnection(con)
}

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfig)
Sys.setenv(R_RAP_INSTANCE = currentInstance)
