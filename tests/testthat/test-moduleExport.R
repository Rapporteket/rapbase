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

config <- yaml::read_yaml(system.file("rapbaseConfig.yml", package = "rapbase"))
currentLogKey <- Sys.getenv("MYSQL_DB_LOG")
dbLogKey <- config$r$raplog$key
Sys.setenv(MYSQL_DB_LOG = dbLogKey)

## shiny session object
session <- list()
attr(session, "class") <- "ShinySession"

## db
test_that("env vars needed for testing is present", {
  check_db()
  expect_true("MYSQL_HOST" %in% names(Sys.getenv()))
  expect_true("MYSQL_USER" %in% names(Sys.getenv()))
  expect_true("MYSQL_PASSWORD" %in% names(Sys.getenv()))
})

# prep db for testing
if (is.null(check_db(is_test_that = FALSE))) {
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
  query_db(query = query)

  create_log_db(dbLogKey)
}

regName <- "rapbase"

with_envvar(
  new = c(
    "FALK_EXTENDED_USER_RIGHTS" = "[{\"A\":80,\"R\":\"LU\",\"U\":1},{\"A\":80,\"R\":\"SC\",\"U\":2},{\"A\":81,\"R\":\"LC\",\"U\":2}]",
    "FALK_APP_ID" = "80"
  ),
  code = {

    test_that("an existing file name is provided", {
      check_db()
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
        check_db()
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
        check_db()
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
  })

test_that("guide test app returns an app object", {
  expect_equal(class(exportUCApp()), "shiny.appobj")
})

# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  query <- c(
    "DROP DATABASE rapbase;",
    paste0("DROP DATABASE IF EXISTS ", dbLogKey, ";")
  )
  query_db(query = query)
}


# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
Sys.setenv(MYSQL_DB_LOG = currentLogKey)
