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
shinySession$request["HTTP_ROLE"] <- "LC"
# make a copy for testing wrong class
shinySessionWrongClass <- shinySession
# simulate ShinySession class for above list
attr(shinySession, "class") <- "ShinySession"

# now deprecated, main function
test_that("shinySessionInfo() is deprecated", {
  expect_error(shinySessionInfo(
    shinySession = shinySession,
    entity = "user"
  ))
})

# now deprecated, wrapper functions
test_that("wrapper funs are deprecated", {
  expect_error(getShinyUserName(shinySession))
  expect_error(getShinyUserGroups(shinySession))
  expect_error(getShinyUserReshId(shinySession))
  expect_error(getShinyUserRole(shinySession))
})
