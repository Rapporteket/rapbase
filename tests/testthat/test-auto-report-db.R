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


Sys.setenv(R_RAP_CONFIG_PATH = tempdir())

file.copy(
  system.file(c("rapbaseConfig.yml"), package = "rapbase"),
  Sys.getenv("R_RAP_CONFIG_PATH")
)
configFile <- file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml")

nameAutoReportDb <- "autoreportTest"

# get some auto report data to work on, i.e. default rapbase
arSample <- yaml::read_yaml(system.file("autoReport.yml", package = "rapbase"))

# make sure we test with database as target
config <- yaml::read_yaml(configFile)
config$r$autoReport$target <- "db"
config$r$autoReport$key <- nameAutoReportDb
yaml::write_yaml(config, configFile)

# adjust config and get whatever name of logging database define there
config$r$autoReport$target <- "db"
yaml::write_yaml(config, configFile)
nameAutoReportDb <- "autoreportTest"

test_that("env vars needed for testing is present", {
  check_db()
  expect_true("DB_HOST" %in% names(Sys.getenv()))
  expect_true("DB_USER" %in% names(Sys.getenv()))
  expect_true("DB_PASS" %in% names(Sys.getenv()))
})

# make temporary config
test_config <- paste0(
  config$r$autoReport$key, ":",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : ", nameAutoReportDb,
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : ephemaralUnitTesting\n"
)

cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)

# helper funs
createAutoReportDb <- function(key) {
  conf <- rapbase::getConfig()
  conf <- conf[[key]]

  query <- paste0("CREATE DATABASE ", conf[["name"]], ";")

  con <- RMariaDB::dbConnect(
    RMariaDB::MariaDB(),
    host = conf$host,
    user = conf$user,
    password = conf$pass
  )
  RMariaDB::dbExecute(con, query)
  RMariaDB::dbDisconnect(con)
}

createAutoReportTab <- function() {
  conf <- getConfig(fileName = "rapbaseConfig.yml")

  fc <- file(system.file("createAutoReportTab.sql", package = "rapbase"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries <- strsplit(sql, ";")[[1]]

  con <- rapOpenDbConnection(conf[["r"]][["autoReport"]][["key"]])[["con"]]
  for (i in seq_len(length(queries))) {
    RMariaDB::dbExecute(con, queries[i])
  }
  rapbase::rapCloseDbConnection(con)
}

test_that("a db for auto report defs can be created", {
  check_db()
  expect_true(createAutoReportDb(nameAutoReportDb))
})

test_that("table can be created in auto report db", {
  check_db()
  expect_null(createAutoReportTab())
})

test_that("a sample of auto report data can be written to db", {
  check_db()
  expect_null(writeAutoReportData(config = arSample))
})

test_that("sample auto report data can be read from db", {
  check_db()
  expect_equal(class(readAutoReportData()), "list")
  expect_identical(readAutoReportData(), upgradeAutoReportData(arSample))
})


# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    bigint = "integer"
  )
  RMariaDB::dbExecute(con, paste("DROP DATABASE", nameAutoReportDb))
  rapbase::rapCloseDbConnection(con)
}

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfig)
Sys.setenv(R_RAP_INSTANCE = currentInstance)
