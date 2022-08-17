context("User info")

# store current instance
currentInstance <- Sys.getenv("R_RAP_INSTANCE")
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

test_that("Function handles general errors", {
  expect_error(userInfo(entity = "user", devContexts = c("DEV", "TEST")))
  expect_error(userInfo(entity = "username"))
})

# Testing on different instances: undefined, DEV, TEST and QA/PRODUCTION
Sys.setenv(R_RAP_INSTANCE = "")
test_that("a message is provided when instance is undefined", {
  expect_message(userInfo(entity = "user"))
})


Sys.setenv(R_RAP_INSTANCE = "DEV")
Sys.setenv(R_RAP_CONFIG_PATH = "")
ss <- list()
# simulate ShinySession class for above list
attr(ss, "class") <- "ShinySession"
test_that("Function provides an entity in a dev context", {
  expect_equal(userInfo(shinySession = ss, entity = "groups"), "rapbase")
})


Sys.setenv(R_RAP_INSTANCE = "TEST")
test_that("Function error when empty shiny session object i TEST context", {
  expect_error(userInfo(entity = "user"))
})

# test case data
ss <- paste0(
  "?X-USER=testUser2&X-GROUPS=testGroup2,",
  "testGroup3&resh_id=78910111&role=SC"
)
shinySessionTest <- list(clientData = list(url_search = ss))

test_that("Function error when shiny session is not of class 'ShinySession'", {
  expect_error(userInfo(entity = "user", shinySession = shinySessionTest))
})

# simulate ShinySession class for above list
attr(shinySessionTest, "class") <- "ShinySession"

test_that("Function provides entities in a TEST context", {
  expect_equal(
    userInfo(entity = "user", shinySession = shinySessionTest),
    "testUser2"
  )
  expect_equal(
    userInfo(entity = "groups", shinySession = shinySessionTest),
    "testGroup2,testGroup3"
  )
  expect_equal(
    userInfo(entity = "resh_id", shinySession = shinySessionTest),
    "78910111"
  )
  expect_equal(userInfo(entity = "role", shinySession = shinySessionTest), "SC")
})


Sys.setenv(R_RAP_INSTANCE = "QA")

# simulated real data
shinySession <- list(user = "user1")
shinySession$groups <- "group1,group2"
shinySession$request <- list(HTTP_RESHID = "789012")
shinySession$request["HTTP_ROLE"] <- "LC"
# make a copy for testing wrong class
shinySessionWrongClass <- shinySession
# simulate ShinySession class for above list
attr(shinySession, "class") <- "ShinySession"

test_that("Function provides entities in a QA/PRODUCTION context", {
  expect_equal(userInfo(entity = "user", shinySession = shinySession), "user1")
  expect_equal(
    userInfo(entity = "groups", shinySession = shinySession),
    "group1,group2"
  )
  expect_equal(
    userInfo(entity = "resh_id", shinySession = shinySession),
    "789012"
  )
  expect_equal(userInfo(entity = "role", shinySession = shinySession), "LC")
})

test_that("unicode chars are properly parsed in QA and PROD contexts", {
  shinySession$request["HTTP_FULLNAME"] <- "åse"
  expect_equal(
    userInfo(entity = "full_name", shinySession = shinySession),
    "åse"
  )
  shinySession$request["HTTP_FULLNAME"] <- "\u00e5se"
  expect_equal(
    userInfo(entity = "full_name", shinySession = shinySession),
    "åse"
  )
  shinySession$request["HTTP_FULLNAME"] <- "\\u00e5se"
  expect_equal(
    userInfo(entity = "full_name", shinySession = shinySession),
    "åse"
  )
})

test_that("Function can handle redefined contexts", {
  expect_equal(userInfo(
    shinySession = shinySession, entity = "user",
    devContexts = c("DEV", "QA"),
    prodContexts = c("PRODUCTION")
  ), "ttester")
  expect_equal(userInfo(
    shinySession = shinySession, entity = "groups",
    devContexts = c("DEV", "QA"),
    prodContexts = c("PRODUCTION")
  ), "rapbase")
  expect_equal(userInfo(
    shinySession = shinySession, entity = "role",
    devContexts = c("DEV", "QA"),
    prodContexts = c("PRODUCTION")
  ), "accessLevel")
  expect_equal(userInfo(
    shinySession = shinySession, entity = "resh_id",
    devContexts = c("DEV", "QA"),
    prodContexts = c("PRODUCTION")
  ), "999999")
})

# Restore instance
Sys.setenv(R_RAP_INSTANCE = currentInstance)
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
