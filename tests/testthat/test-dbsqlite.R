test_that("A sqlite db connection and driver can be provided and cleaned", {
    check_db()
    l <- rapOpenDbConnection(dbName = ":memory:", dbType = "sqlite")
    expect_output(str(l), "List of 2")
    expect_is(l[[1]], "SQLiteConnection")
    expect_is(l[[2]], "SQLiteDriver")
    expect_true(DBI::dbIsValid(l$con))
    rapCloseDbConnection(l$con)
    expect_false(DBI::dbIsValid(l$con))
    l <- NULL
})
