context("Handling db connections")

test_that("Error provided when key has no corresponding config", {
  expect_error(rapOpenDbConnection(registryName = "aNoneExistingRegistryKey"))
})

test_that("A mysql db connection and driver can be provided and cleaned", {
  l <- rapOpenDbConnection("rapbase")
  expect_output(str(l), "List of 2")
  expect_is(l[[1]], "MySQLConnection")
  expect_is(l[[2]], "MySQLDriver")
  expect_true(RMySQL::isIdCurrent(l$con))
  rapCloseDbConnection(l$con)
  expect_error(RMySQL::isIdCurrent(l$con))
})

test_that("Data can be queried from (MySQL) db", {
  query <- "SELECT * FROM RapbaseTestTable"
  expect_output(str(LoadRegData("rapbase", query, dbType = "mysql")),
                "data.frame")
})