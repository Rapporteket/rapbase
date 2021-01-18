tempdir <- tempdir()

test_that("error is provided when archive dir already exists", {
	expect_error(createArchive(archivePath = tempdir))
})

test_that("an archive dir can be created", {
	expect_true(createArchive(archivePath = tempfile()))
})

test_that("error is provided when archive dir does not exist", {
	expect_error(archiveLog(archivePath = "", logPath = tempdir))
})

test_that("error is provided when log dir does not exist", {
	expect_error(archiveLog(archivePath = tempdir, logPath = ""))
})

test_that("error is provided when log files does not exixt", {
	expect_error(archiveLog(archivePath = tempdir, logPath = tempdir,
													logs = c("foo.csv", "bar.csv")))
})


logs <- c("mtcars.csv")
rio::export(mtcars, file.path(tempdir, logs))

test_that("an none-existing oversized log returns character()", {
	expect_equal(logsOverSize(logPath = tempdir, overSize = 1,
														pattern = "not_existing_at_all.csv"), character())
})

test_that("an oversized log can be detected", {
	expect_equal(logsOverSize(logPath = tempdir, overSize = 1,
														pattern = ".csv$"), logs)
})

test_that("nothing is archived when list of logs is empty", {
	expect_null(archiveLog(archivePath = tempdir, logPath = tempdir, logs = character()))
})

test_that("function silently archives...", {
	expect_silent(archiveLog(archivePath = tempdir, logPath = tempdir,
													 logs = logs))
})

test_that("source log was actually removed when archiving", {
	expect_false(file.exists(logs))
})

# now, since we time tagged the archived file, get its name
archivedFile <- list.files(path = tempdir, pattern = "mtcars.rda$")

test_that("...and that archive actually exists", {
	expect_true(file.exists(file.path(tempdir, archivedFile)))
})

test_that("no cleaning is performed when eol is set to 0", {
	expect_null(cleanArchive(archivePath = tempdir))
})

test_that("function silently cleans...", {
	expect_silent(cleanArchive(archivePath = tempdir, eolDays = 0))
})

test_that("...and that file is actually removed", {
	expect_false(file.exists(file.path(tempdir, "mtcars.rda")))
})
