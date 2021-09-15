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
file.copy(system.file(c("rapbaseConfig.yml", "dbConfig.yml"),
                      package = "rapbase"),
          Sys.getenv("R_RAP_CONFIG_PATH"))

## shiny session object
session <- list()
attr(session, "class") <- "ShinySession"

## db
# Database infrastructure is only available at gh actions and our own dev env.
# Tests running on other environments should be skipped
checkDb <- function() {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    testthat::skip("Test skipped due to lack of database infrastructure")
  }
}

# Make sure we do have a test db during DEV
# On gh actions, this will be taken care of in config (coverage.yml)
# If in a DEV context alter the default setup
regName <- "rapbase"
if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
  regName <- "dev"
  query <- c(
    "DROP DATABASE IF EXISTS rapbase;",
    "CREATE DATABASE rapbase;",
    "USE rapbase;",
    paste(
      "CREATE TABLE testTable (id int, someText varchar(50),",
      "someInt INT, someBigInt BIGINT, someFloat DOUBLE,",
      "someTime DATETIME);"
    )
  )
  conf <- getConfig()[[regName]]
  drv <- RMariaDB::MariaDB()
  con <- DBI::dbConnect(drv,
                        host = conf$host,
                        user = conf$user,
                        password = conf$pass
  )
  for (q in query) {
    tmp <- DBI::dbExecute(con, q)
  }
  DBI::dbDisconnect(con)
}


test_that("an existing file name is provided", {
  checkDb()
  f <- exportDb(regName, compress = TRUE, session = session)
  expect_true(file.exists(f))
})


# The remaining test the corresponding shiny modules
test_that("export UC input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(exportUCInput("id")))
})

test_that("guide module server provides sensible output", {
  shiny::testServer(exportUCServer, args = list(registryName = "rapbase"), {
    expect_equal(class(output$exportPidUI), "list")
    session$setInputs(exportPid = "areedv")
    expect_equal("character", class(pubkey()))
    session$setInputs(exportKey = pubkey())
    expect_equal(class(output$exportKeyUI), "list")
    session$setInputs(exportCompress = TRUE)
    expect_true(is.null(rv$exportFile))
    expect_equal(class(output$exportEncryptUI), "list")
  })
})

test_that("download is prevented when module is not eligible", {
  checkDb()
  shiny::testServer(
    exportUCServer,
    args = list(registryName = regName, eligible = FALSE), {
      session$setInputs(exportPid = "areedv")
      session$setInputs(exportKey = pubkey())
      session$setInputs(exportCompress = TRUE)
      session$setInputs(exportEncrypt = 1)
      expect_false(exists("output$exportDownload"))
    })
})

test_that("guide test app returns an app object", {
  expect_equal(class(exportUCApp()), "shiny.appobj")
})

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
