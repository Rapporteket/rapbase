test_that("a abbreviated named list can be provided from key(s)", {
  testKey <- "ssh-rsa averylongstring"
  testName <- "ssh-rsa ...ngstring"
  expect_equal(names(selectListPubkey(testKey)), testName)
})

# rest of test on db export will have to be performed where a db is present,
# a valid shiny session object and somewhere to do logging

## store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
file.copy(system.file(c("rapbaseConfig.yml"), package = "rapbase"),
  Sys.getenv("R_RAP_CONFIG_PATH"),
  overwrite = TRUE
)

## shiny session object
session <- list()
attr(session, "class") <- "ShinySession"

## db
# Database infrastructure is only available at gh actions and our own dev env.
# Tests running on other environments should be skipped
checkDb <- function(is_test_that = TRUE) {
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

test_that("env vars needed for testing is present", {
  checkDb()
  expect_true("DB_HOST" %in% names(Sys.getenv()))
  expect_true("DB_USER" %in% names(Sys.getenv()))
  expect_true("DB_PASS" %in% names(Sys.getenv()))
})

# prep db for testing
if (is.null(checkDb(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    bigint = "integer"
  )
  RMariaDB::dbExecute(con, "CREATE DATABASE rapbase;")
  RMariaDB::dbDisconnect(con)
}

if (is.null(checkDb(is_test_that = FALSE))) {
  query <- c(
    "USE rapbase;",
    paste(
      "CREATE TABLE testTable (id int, someText varchar(50),",
      "someInt INT, someBigInt BIGINT, someFloat DOUBLE,",
      "someTime DATETIME);"
    )
  )
  drv <- RMariaDB::MariaDB()
  con <- RMariaDB::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    bigint = "integer"
  )
  for (q in query) {
    tmp <- DBI::dbExecute(con, q)
  }
  DBI::dbDisconnect(con)
}

# make temporary config
regName <- "rapbase"
test_config <- paste0(
  "rapbase:",
  "\n  host : ", Sys.getenv("DB_HOST"),
  "\n  name : rapbase",
  "\n  user : ", Sys.getenv("DB_USER"),
  "\n  pass : ", Sys.getenv("DB_PASS"),
  "\n  disp : ephemaralUnitTesting\n"
)
# preserve initial state
cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)


test_that("an existing file name is provided", {
  checkDb()
  f <- exportDb(regName, compress = TRUE, session = session)
  expect_true(file.exists(f))
})


# The remaining test the corresponding shiny modules
test_that("export UC input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(exportUCInput("id")))
})

# To recreate the stored responses delete the 'gh_api_response' directory
# recursively and re-run these tests

with_mock_dir("gh_api_response", {
  test_that("module server provides sensible output", {
    checkDb()
    shiny::testServer(exportUCServer, args = list(registryName = "rapbase"), {
      expect_equal(class(output$exportPidUI), "list")
      session$setInputs(exportPid = "areedv")
      expect_equal("character", class(pubkey()))
      session$setInputs(exportKey = pubkey())
      expect_equal(class(output$exportKeyUI), "list")
      session$setInputs(exportCompress = FALSE)
      expect_true(length(encFile()) == 1)
      session$setInputs(exportDownload = 1)
      expect_true(basename(output$exportDownload) == basename(encFile()))
    })
  })

  test_that("download is prevented when module is not eligible", {
    checkDb()
    shiny::testServer(
      exportUCServer,
      args = list(registryName = regName, eligible = FALSE),
      {
        session$setInputs(exportPid = "areedv")
        session$setInputs(exportKey = pubkey())
        session$setInputs(exportCompress = TRUE)
        session$setInputs(exportEncrypt = 1)
        expect_false(exists("output$exportDownload"))
      }
    )
  })
})

test_that("guide test app returns an app object", {
  expect_equal(class(exportUCApp()), "shiny.appobj")
})

# remove test db
if (is.null(checkDb(is_test_that = FALSE))) {
  con <- rapbase::rapOpenDbConnection(regName)$con
  RMariaDB::dbExecute(con, "DROP DATABASE rapbase;")
  rapbase::rapCloseDbConnection(con)
}


# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
