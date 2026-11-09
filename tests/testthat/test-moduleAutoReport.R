## store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

# make pristine and dedicated config to avoid interference with other tests
Sys.setenv(R_RAP_CONFIG_PATH = file.path(tempdir(), "autoReportTesting"))
dir.create(Sys.getenv("R_RAP_CONFIG_PATH"), showWarnings = FALSE)
file.copy(
  system.file(c("rapbaseConfig.yml", "autoReport.yml"),
    package = "rapbase"
  ),
  Sys.getenv("R_RAP_CONFIG_PATH"),
  overwrite = TRUE
)

registryName <- "rapbase"

test_that("auto report UI returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(autoReportUI("id")))
})

test_that("report file is served through a session-scoped data object", {
  captured <- list()
  session <- list(
    registerDataObj = function(name, data, filterFunc) {
      captured <<- list(name = name, data = data, filterFunc = filterFunc)
      "session/token/rapbase_check"
    }
  )

  htmlFile <- tempfile(fileext = ".html")
  writeLines("<p>hei</p>", htmlFile)
  otherFile <- tempfile(fileext = ".html")
  writeLines("<p>secret</p>", otherFile)

  expect_equal(
    rapbase:::.registerReportFile(session, htmlFile),
    "session/token/rapbase_check"
  )
  resp <- captured$filterFunc(captured$data, NULL)
  expect_equal(resp$status, 200L)
  expect_match(resp$content_type, "text/html")
  expect_equal(trimws(rawToChar(resp$content)), "<p>hei</p>")
  expect_false(grepl("secret", rawToChar(resp$content)))

  pdfFile <- tempfile(fileext = ".pdf")
  writeLines("%PDF", pdfFile)
  rapbase:::.registerReportFile(session, pdfFile)
  expect_equal(captured$data$type, "application/pdf")

  txtFile <- tempfile(fileext = ".txt")
  writeLines("text", txtFile)
  rapbase:::.registerReportFile(session, txtFile)
  expect_match(captured$data$type, "text/plain")
})

test_that("auto report Org input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(autoReportOrgInput("id")))
})

test_that("auto report Format input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(autoReportFormatInput("id")))
})

test_that("auto report input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(autoReportInput("id")))
})


test_that("module org server returns outputs and list of reactives", {
  orgs <- list(
    OrgOne = 100082,
    OrgTwo = 102966
  )
  shiny::testServer(autoReportOrgServer, args = list(orgs = orgs), {
    session$setInputs(org = 111111)
    expect_equal(class(output$orgs), "list")
    expect_equal(class(session$returned), "list")
    expect_true(shiny::is.reactive(session$returned$name))
    expect_true(shiny::is.reactive(session$returned$value))
  })
})

test_that("module format server returns outputs and list of reactives", {
  shiny::testServer(autoReportFormatServer, {
    expect_equal(class(output$format), "list")
    expect_true(shiny::is.reactive(session$returned))
  })
})

# Helpers for testing the "check report" button without a database or a real
# browser session. Variable names are prefixed with 'chk_'/'mod_' to avoid
# being masked by objects in the module server environment.
makeCheckData <- function(
  id, type = "dispatchment", owner = "ttester", org = "999999",
  fun = ".testAutoReport",
  params = '{"aNum":1,"aChar":"a","anExp":"x"}'
) {
  data.frame(
    id = id,
    synopsis = paste("Rapport", id),
    package = "rapbase",
    type = type,
    fun = fun,
    params = params,
    owner = owner,
    ownerName = "Tore Tester",
    email = "someone@nowhere.com",
    organization = org,
    startDate = as.Date("2024-01-01"),
    terminateDate = as.Date("2099-12-31"),
    interval = "months",
    intervalName = "M\u00E5nedlig",
    runDayOfYear = "30,60"
  )
}

pressCheck <- function(
  chk_id, chk_data, mod_type = "dispatchment", mod_eligible = TRUE,
  mod_getFun = NULL
) {
  cap <- new.env()
  cap$modal <- NULL
  cap$notification <- NULL
  cap$path <- NULL
  cap$messages <- character()

  testthat::local_mocked_bindings(
    readAutoReportData = function(...) chk_data,
    .registerReportFile = function(session, path) {
      cap$path <- path
      "mock/url"
    },
    .package = "rapbase"
  )
  if (!is.null(mod_getFun)) {
    testthat::local_mocked_bindings(.getFun = mod_getFun, .package = "rapbase")
  }
  testthat::local_mocked_bindings(
    showModal = function(ui, session = NULL) cap$modal <- as.character(ui),
    showNotification = function(ui, ..., type = "default") {
      cap$notification <- list(ui = ui, type = type)
    },
    withProgress = function(expr, ...) expr,
    .package = "shiny"
  )

  mod_user <- list(
    name = shiny::reactiveVal("ttester"),
    fullName = shiny::reactiveVal("Tore Tester"),
    email = shiny::reactiveVal("someone@nowhere.com"),
    org = shiny::reactiveVal(999999),
    role = shiny::reactiveVal("LC")
  )

  withCallingHandlers(
    shiny::testServer(
      autoReportServer,
      args = list(
        registryName = "rapbase", type = mod_type,
        org = shiny::reactive(999999),
        orgs = list(OrgOne = 999999),
        eligible = shiny::reactiveVal(mod_eligible),
        user = mod_user
      ),
      {
        session$setInputs(
          check_button = list(
            id = session$ns(paste0("check__", chk_id)),
            nonce = 1
          )
        )
      }
    ),
    message = function(m) {
      cap$messages <- c(cap$messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  as.list(cap)
}

test_that("check button logs the report id", {
  res <- pressCheck(1, makeCheckData(1))
  expect_true("Check report for id: 1" %in% trimws(res$messages))
})

test_that("check button shows a file report in an iframe", {
  res <- pressCheck(1, makeCheckData(1))
  expect_null(res$notification)
  expect_true(file.exists(res$path))
  expect_match(res$modal, "<iframe", fixed = TRUE)
  expect_match(res$modal, "mock/url", fixed = TRUE)
  expect_match(res$modal, "Sjekk rapport: Rapport 1", fixed = TRUE)
})

test_that("check button resolves fun given as 'package::function'", {
  res <- pressCheck(
    1,
    makeCheckData(1, fun = "rapbase::.testAutoReport")
  )
  expect_true(file.exists(res$path))
  expect_match(res$modal, "<iframe", fixed = TRUE)
})

test_that("check button uses the first id of a grouped entry", {
  data <- rbind(makeCheckData(1), makeCheckData(2))
  res <- pressCheck("2\n1", data)
  expect_match(res$modal, "Sjekk rapport: Rapport 2", fixed = TRUE)
})

test_that("check button shows a bulletin as html", {
  data <- makeCheckData(
    1, type = "bulletin",
    params = '{"aNum":1,"aChar":"a","anExp":"x","bulletin":1}'
  )
  res <- pressCheck(1, data, mod_type = "bulletin")
  expect_null(res$path)
  expect_match(res$modal, "This is a simple test of automated reports")
  expect_false(grepl("<iframe", res$modal, fixed = TRUE))
})

test_that("check button shows html content as is", {
  res <- pressCheck(
    1, makeCheckData(1),
    mod_getFun = function(x) {
      function(...) structure("<p>hei</p>", class = "html")
    }
  )
  expect_null(res$path)
  expect_match(res$modal, "<p>hei</p>", fixed = TRUE)
})

test_that("check button shows an error when the report function fails", {
  res <- pressCheck(
    1, makeCheckData(1),
    mod_getFun = function(x) function(...) stop("boom")
  )
  expect_match(res$modal, "alert-danger", fixed = TRUE)
  expect_match(res$modal, "Rapporten kunne ikke lages: boom", fixed = TRUE)
})

test_that("check button shows an error when report output is not a file", {
  res <- pressCheck(
    1, makeCheckData(1),
    mod_getFun = function(x) function(...) "no/such/file"
  )
  expect_null(res$path)
  expect_match(res$modal, "alert-danger", fixed = TRUE)
  expect_match(res$modal, "Rapporten kunne ikke lages.", fixed = TRUE)
})

test_that("check button notifies when the report is not found", {
  res <- pressCheck(99, makeCheckData(1))
  expect_null(res$modal)
  expect_equal(res$notification$ui, "Fant ikke rapporten.")
  expect_equal(res$notification$type, "error")
})

test_that("check button only finds reports of the right type and package", {
  res <- pressCheck(1, makeCheckData(1, type = "bulletin"))
  expect_null(res$modal)
  expect_equal(res$notification$type, "error")

  data <- makeCheckData(1)
  data$package <- "otherpackage"
  res <- pressCheck(1, data)
  expect_null(res$modal)
  expect_equal(res$notification$type, "error")
})

test_that("check button for subscriptions is limited to owner and org", {
  res <- pressCheck(
    1, makeCheckData(1, type = "subscription"),
    mod_type = "subscription"
  )
  expect_match(res$modal, "Sjekk rapport: Rapport 1", fixed = TRUE)

  res <- pressCheck(
    1, makeCheckData(1, type = "subscription", owner = "someoneelse"),
    mod_type = "subscription"
  )
  expect_null(res$modal)
  expect_equal(res$notification$type, "error")

  res <- pressCheck(
    1, makeCheckData(1, type = "subscription", org = "111111"),
    mod_type = "subscription"
  )
  expect_null(res$modal)
  expect_equal(res$notification$type, "error")
})

test_that("check button does nothing when module is not eligible", {
  res <- pressCheck(1, makeCheckData(1), mod_eligible = FALSE)
  expect_null(res$modal)
  expect_null(res$notification)
  expect_null(res$path)
})

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
