context("Shiny session info")

# test case data
ss <- "?X-USER=testUser1&X-GROUPS=testGroup1,testGroup2&resh_id=123456&role=LU"
shinySessionTest <- list(clientData = list(url_search = ss))

# simulate ShinySession class for above list
attr(shinySessionTest, "class") <- "ShinySession"

# simulated real data
shinySession <- list(user = "user1")
shinySession$groups <- "group1,group2"
shinySession$request <- list(HTTP_RESH_ID = "789012")
shinySession$request$HTTP_ROLE <- "LC"
# make a copy for testing wrong class
shinySessionWrongClass <- shinySession
# simulate ShinySession class for above list
attr(shinySession, "class") <- "ShinySession"

# now deprecated, main function
test_that("shinySessionInfo() is deprecated", {
  expect_warning(shinySessionInfo(
    shinySession = shinySession,
    entity = "user"
  ))
})

# now deprecated, wrapper functions
test_that("wrapper funs are deprecated", {
  expect_warning(getShinyUserName(shinySession),
    class = "lifecycle_warning_deprecated"
  )
  expect_warning(getShinyUserGroups(shinySession),
    class = "lifecycle_warning_deprecated"
  )
  expect_warning(getShinyUserReshId(shinySession),
    class = "lifecycle_warning_deprecated"
  )
  expect_warning(getShinyUserRole(shinySession),
    class = "lifecycle_warning_deprecated"
  )
})

# now deprecated, but should still work
withr::local_options(list(lifecycle_verbosity = "quiet"))

test_that("Default is to assume real data scenario", {
  expect_equal(getShinyUserName(shinySession), "user1")
  expect_equal(getShinyUserGroups(shinySession), "group1,group2")
  expect_equal(getShinyUserReshId(shinySession), "789012")
  expect_equal(getShinyUserRole(shinySession), "LC")
})

test_that("Also working for test cases", {
  expect_equal(
    getShinyUserName(shinySessionTest, testCase = TRUE),
    "testUser1"
  )
  expect_equal(
    getShinyUserGroups(shinySessionTest, testCase = TRUE),
    "testGroup1,testGroup2"
  )
  expect_equal(
    getShinyUserReshId(shinySessionTest, testCase = TRUE),
    "123456"
  )
  expect_equal(getShinyUserRole(shinySessionTest, testCase = TRUE), "LU")
})

test_that("Function handle errors", {
  expect_error(shinySessionInfo(shinySession = NULL, entity = "user"))
  expect_error(shinySessionInfo(
    shinySession = shinySessionWrongClass,
    entity = "user"
  ))
  expect_error(shinySessionInfo(
    shinySession = shinySession,
    entity = ""
  ))
})
