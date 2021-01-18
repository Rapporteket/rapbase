# store current instance and prepare
currentInstance <- Sys.getenv("R_RAP_INSTANCE")
currentConfig <- Sys.getenv("R_RAP_CONFIG_PATH")


tempdir <- tempdir()

file.copy(system.file("rapbaseConfig.yml", package = "rapbase"), tempdir)


Sys.setenv(R_RAP_INSTANCE="DEV")
Sys.setenv(R_RAP_CONFIG_PATH=tempdir)

session <- list()
attr(session, "class") <- "ShinySession"



test_that("logging is performed at application level", {
	expect_silent(appLogger(session))
	expect_true(file.exists(file.path(tempdir, "appLog.csv")))
})

test_that("logging is performed at report level", {
	expect_silent(repLogger(session))
	expect_true(file.exists(file.path(tempdir, "reportLog.csv")))
})

test_that("logging can be made by (automated) reports outdise session", {
	expect_silent(autLogger(user = "ttest", registryName = "rapbase",
													reshId = "999999"))
})

test_that("formatter returns error upon non-existing format", {
	expect_error(makeLogRecord(content = list(), format = "a4"))
})

test_that("formatter returns a data.frame-class object", {
	expect_equal(class(makeLogRecord(content = list())), "data.frame")
})

test_that("formatter returns as expected", {
	expect_equal(makeLogRecord(list(val=0))$val, 0)
})

# must be last...
Sys.setenv(R_RAP_CONFIG_PATH="")

test_that("nothing will be appended if path is not defined", {
	expect_error(appendLog(event=data.frame(foo="bar"), name="", target="file", format="csv"))
})

test_that("nothing will be appended if (file) format is not recognized", {
	expect_error(appendLog(event=data.frame(foo="bar"), name="", target="file", format="a4"))
})

test_that("error is provided when target is not supported", {
	expect_error(appendLog(event=data.frame(foo="bar"), name="", target="moon", format="a4"))
})


# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH=currentConfig)
Sys.setenv(R_RAP_INSTANCE=currentInstance)
