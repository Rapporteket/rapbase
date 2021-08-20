## store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

# make pristine config path to avoid clutter from other tests
Sys.setenv(R_RAP_CONFIG_PATH = file.path(tempdir(), "autoReportTesting"))
dir.create(Sys.getenv("R_RAP_CONFIG_PATH"))
file.copy(system.file(c("rapbaseConfig.yml", "dbConfig.yml", "autoReport.yml"),
                      package = "rapbase"),
          Sys.getenv("R_RAP_CONFIG_PATH"))

registryName <- "rapbase"

test_that("auto report input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(autoReportInput("id")))
})

test_that("auto report UI returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(autoReportUI("id")))
})

# prep arguments
## make a list for report metadata
reports <- list(
  FirstReport = list(
    synopsis = "First example report",
    fun = "fun1",
    paramNames = c("a", "b"),
    paramValues = c(1, "yes")
  ),
  SecondReport = list(
    synopsis = "Second example report",
    fun = "fun2",
    paramNames = "x",
    paramValues = 0
  )
)

## make a list of organization names and numbers
orgs <- list(
  OrgOne = 111111,
  OrgTwo = 222222
)

## set type
type <- "subscription"

test_that("module server provides sensible output", {
  shiny::testServer(autoReportServer,
                    args = list(registryName = registryName, type = type,
                                reports = reports, orgs = orgs), {
    session$setInputs(report = "FirstReport")
    expect_equal(class(output$reports), "list")

  })
})

# session$setInputs(period = rep(Sys.time(), 2), downloadFormat = "csv")
# expect_equal(class(output$download), "character")
# session$setInputs(downloadFormat = "xlsx-csv")
# expect_true(file.exists(output$download))

test_that("test app returns an app object", {
  expect_equal(class(autoReportApp()), "shiny.appobj")
})

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
