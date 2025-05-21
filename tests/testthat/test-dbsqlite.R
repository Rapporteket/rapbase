test_that("A sqlite db connection and driver can be provided and cleaned", {
    l <- rapOpenDbConnection(dbName = ":memory:", dbType = "sqlite")
    expect_output(str(l), "List of 2")
    expect_is(l[[1]], "SQLiteConnection")
    expect_is(l[[2]], "SQLiteDriver")
    expect_true(DBI::dbIsValid(l$con))
    rapCloseDbConnection(l$con)
    expect_false(DBI::dbIsValid(l$con))
    l <- NULL
})

test_that("Some data can be added to a sqlite db", {
  con <- rapOpenDbConnection(dbName = ":memory:", dbType = "sqlite")$con
  DBI::dbWriteTable(con, "mtcars", mtcars)
  DBI::dbWriteTable(con, "iris", iris)
  expect_equal(DBI::dbListTables(con), c("iris", "mtcars"))
  rapCloseDbConnection(con)
  con <- NULL
})

test_that("Some data can be read from a sqlite db file with loadRegData", {
  someData <- loadRegData(
    registryName = "data/db.sqlite",
    'SELECT gear FROM mtcars WHERE disp = 360 AND hp = 175',
    dbType = "sqlite"
  )
  expect_equal(someData$gear,3)
})
