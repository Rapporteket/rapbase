# store current instance and prepare
currentInstance <- Sys.getenv("R_RAP_INSTANCE")
currentConfig <- Sys.getenv("R_RAP_CONFIG_PATH")

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
  expect_true("MYSQL_HOST" %in% names(Sys.getenv()))
  expect_true("MYSQL_USER" %in% names(Sys.getenv()))
  expect_true("MYSQL_PASSWORD" %in% names(Sys.getenv()))
})

# helper funs
createAutoReportDb <- function() {
  query <- paste0("CREATE DATABASE ", nameAutoReportDb, ";")
  query_db(query = query)
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
  con <- NULL
}

test_that("a db for auto report defs can be created", {
  check_db()
  expect_null(query_db(query =  paste0("CREATE DATABASE ", nameAutoReportDb, ";")))
})

test_that("table can be created in auto report db", {
  check_db()
  expect_null(createAutoReportTab())
})

test_that("a sample of auto report data can be written to db", {
  check_db()
  expect_null(writeAutoReportData(config = arSample, target = "db"))
})

test_that("sample auto report data can be read from db", {
  check_db()
  expect_equal(nrow(readAutoReportData(target = "db")), 7)
  expect_equal(class(readAutoReportData(target = "db")), "data.frame")
  writeAutoReportData(config = arSample, target = "db")
  expect_equal(nrow(readAutoReportData(target = "db")), 14)
})

# For a valid test make sure there is ONE standard dummy report scheduled
# monthly and with start date first of some month.
# 1 January 1900 is a Monday
# 1 December 2024 is a Sunday, but given number of months from start date
test_that("Auto reports can be processed monthly", {
  check_db()
  expect_message(runAutoReport(
    dato = "2024-12-01",
    dryRun = TRUE,
    target = "db"
    ),
    "No emails sent. Content is:",
    all = FALSE
  )
})

# 1 January 1900 is a Monday
# 2 December 2024 is a Monday
test_that("Auto reports can be processed weekly", {
  check_db()
  expect_message(runAutoReport(
    dato = "2024-12-02",
    dryRun = TRUE,
    target = "db"
    ),
    "No emails sent. Content is:",
    all = FALSE
  )
})

# Do the same for a bulletin, above conditions also apply!
test_that("Bulletin reports can be processed (monthly)", {
  check_db()
  expect_message(
    runAutoReport(
      dato = "2024-12-01",
      type = c("bulletin"),
      dryRun = TRUE,
      target = "db"
      ),
    "No emails sent. Content is: This is a simple",
    all = FALSE
  )
})

test_that("Auto reports not sent because of no reports this date", {
  check_db()
  expect_silent(runAutoReport(
    dato = "2024-12-03",
    dryRun = TRUE,
    target = "db"
    ))
})

test_that("Auto reports not sent if before start date", {
  check_db()
  expect_silent(runAutoReport(
    dato = "1800-01-01",
    dryRun = TRUE,
    target = "db"
    ))
})

test_that("Auto reports not sent if after start date", {
  check_db()
  expect_silent(runAutoReport(
    dato = "3000-01-01",
    dryRun = TRUE,
    target = "db"
    ))
})


# Test autoReportServer2 with db.
# Tests are copied from test-moduleAutoReport.R since the current file
# is ready for db.
if (is.null(check_db(is_test_that = FALSE))) {
  withr::with_envvar(
    new = c(
      "FALK_EXTENDED_USER_RIGHTS" = "[{\"A\":80,\"R\":\"LC\",\"U\":1},{\"A\":80,\"R\":\"SC\",\"U\":2},{\"A\":81,\"R\":\"LC\",\"U\":2}]",
      "FALK_APP_ID" = "80"
    ),
    code = {
      registryName <- "autoreportTest"
      ## make a list for report metadata
      reports <- list(
        FirstReport = list(
          synopsis = "First example report",
          fun = "fun1",
          paramNames = c("organization", "outputFormat"),
          paramValues = c(100082, "html")
        ),
        SecondReport = list(
          synopsis = "Second example report",
          fun = "fun2",
          paramNames = c("organization", "outputFormat"),
          paramValues = c(102966, "pdf")
        )
      )
      ## make a list of organization names and numbers
      orgs <- list(
        OrgOne = 100082,
        OrgTwo = 102966
      )
      type <- "subscription"
      user <- userAttribute(unit = 1)
      for (n in names(user)) {
        user[[n]] <- shiny::reactiveVal(user[[n]])
      }

      test_that("module server provides sensible output", {
        shiny::testServer(autoReportServer,
                          args = list(
                            registryName = registryName, type = type,
                            reports = reports, orgs = orgs, user = user
                          ),
                          {
                            session$setInputs(report = "FirstReport")
                            expect_equal(class(output$reports), "list")
                          }
        )
      })

      test_that("no report select list created when no reports available", {
        shiny::testServer(
          autoReportServer,
          args = list(
            registryName = registryName, type = type,
            reports = NULL, orgs = orgs, user = user
          ),
          {
            expect_true(is.null(output$reports))
          }
        )
      })
      type <- "dispatchment"
      test_that("email can be added and deleted for dispatchment", {
        shiny::testServer(
          autoReportServer,
          args = list(
            registryName = registryName, type = type,
            org = shiny::reactive(100082),
            reports = reports, orgs = orgs, user = user
          ),
          {
            session$setInputs(email = "true@email.no")
            expect_equal(length(autoReport$email), 0)
            session$setInputs(addEmail = 1)
            expect_equal(autoReport$email[1], "true@email.no")
            session$setInputs(delEmail = 1)
            expect_equal(length(autoReport$email), 0)
          }
        )
      })


      test_that("add email button is not created if email is not valid", {
        shiny::testServer(
          autoReportServer,
          args = list(
            registryName = registryName, type = type,
            org = shiny::reactive(100082),
            reports = reports, orgs = orgs, user = user
          ),
          {
            session$setInputs(email = "invalid@email-format")
            expect_true(is.null(output$editEmail))
            session$setInputs(email = "invalid@email-format.o")
            expect_true(is.null(output$editEmail))
            session$setInputs(email = "invalid.email-format.on")
            expect_true(is.null(output$editEmail))
          }
        )
      })

      test_that("no submit button is provided when module is not eligible", {
        shiny::testServer(
          autoReportServer,
          args = list(
            registryName = registryName, type = "subscription",
            reports = reports, orgs = orgs, eligible = shiny::reactiveVal(FALSE), user = user
          ),
          {
            session$setInputs(email = "valid.email@format.no")
            expect_true(is.null(output$makeAutoReport))
          }
        )
      })

    }
  )
}


# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("MYSQL_HOST"),
    user = Sys.getenv("MYSQL_USER"),
    password = Sys.getenv("MYSQL_PASSWORD"),
    bigint = "integer"
  )
  RMariaDB::dbExecute(con, paste("DROP DATABASE", nameAutoReportDb))
  rapbase::rapCloseDbConnection(con)
}

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfig)
Sys.setenv(R_RAP_INSTANCE = currentInstance)
