## store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

# make pristine and dedicated config to avoid interference with other tests
Sys.setenv(R_RAP_CONFIG_PATH = file.path(tempdir(), "autoReportTesting"))
dir.create(Sys.getenv("R_RAP_CONFIG_PATH"))
file.copy(
  system.file(c("rapbaseConfig.yml", "autoReport.yml"),
    package = "rapbase"
  ),
  Sys.getenv("R_RAP_CONFIG_PATH")
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
  expect_equal(rawToChar(resp$content), "<p>hei</p>\n")
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

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
