context("User info helper functions")

# store current instance
currentInstance <- Sys.getenv("R_RAP_INSTANCE")

# take whatever user info provided by confing
conf <- getConfig(fileName = "rapbaseConfig.yml")
d <- conf$r$testUser

Sys.setenv(R_RAP_INSTANCE="DEV")
test_that("Helpers provide config data when no session data present", {
  expect_equal(getUserName(), d$user)
  expect_equal(getUserGroups(), d$groups)
  expect_equal(getUserReshId(), d$resh_id)
  expect_equal(getUserRole(), d$role)
})


# Simulated session
shinySession <- list(user="user1")
shinySession$groups <- "group1,group2"
shinySession$request <- list(HTTP_RESH_ID="789012")
shinySession$request$HTTP_ROLE <- "LC"
# simulate ShinySession class for above list
attr(shinySession, "class") <- "ShinySession"

Sys.setenv(R_RAP_INSTANCE="PRODUCTION")
test_that("Helpers provide session data", {
  expect_equal(getUserName(shinySession), "user1")
  expect_equal(getUserGroups(shinySession), "group1,group2")
  expect_equal(getUserReshId(shinySession), "789012")
  expect_equal(getUserRole(shinySession), "LC")
})


# Restore instance
Sys.setenv(R_RAP_INSTANCE=currentInstance)