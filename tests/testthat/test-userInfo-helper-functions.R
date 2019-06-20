context("User info helper functions")

# store current instance
currentInstance <- Sys.getenv("R_RAP_INSTANCE")

Sys.setenv(R_RAP_INSTANCE="DEV")
test_that("Helpers provide warning when no session data present", {
  expect_warning(getUserName())
  expect_warning(getUserGroups())
  expect_warning(getUserReshId())
  expect_warning(getUserRole())
  expect_warning(getUserEmail())
  expect_warning(getUserFullName())
  expect_warning(getUserPhone())
})

# take whatever user info provided by confing
conf <- getConfig(fileName = "rapbaseConfig.yml")
d <- conf$r$testUser

Sys.setenv(R_RAP_INSTANCE="DEV")
test_that("Helpers provide config data when provided session object is empty", {
  expect_equal(getUserName(shinySession = NULL), d$user)
  expect_equal(getUserGroups(shinySession = NULL), d$groups)
  expect_equal(getUserReshId(shinySession = NULL), d$resh_id)
  expect_equal(getUserRole(shinySession = NULL), d$role)
  expect_equal(getUserEmail(shinySession = NULL), d$email)
  expect_equal(getUserFullName(shinySession = NULL), d$full_name)
  expect_equal(getUserPhone(shinySession = NULL), d$phone)
})

# Simulated session
shinySession <- list(user="user1")
shinySession$groups <- "group1,group2"
shinySession$request <- list(HTTP_RESHID="789012")
shinySession$request$HTTP_ROLE <- "LC"
shinySession$request$HTTP_EMAIL <- "user1@nowhere.no"
shinySession$request$HTTP_FULLNAME <- "T Test"
shinySession$request$HTTP_PHONE <- "04050607"
# simulate ShinySession class for above list
attr(shinySession, "class") <- "ShinySession"

Sys.setenv(R_RAP_INSTANCE="PRODUCTION")
test_that("Helpers provide session data", {
  expect_equal(getUserName(shinySession), "user1")
  expect_equal(getUserGroups(shinySession), "group1,group2")
  expect_equal(getUserReshId(shinySession), "789012")
  expect_equal(getUserRole(shinySession), "LC")
  expect_equal(getUserEmail(shinySession), "user1@nowhere.no")
  expect_equal(getUserFullName(shinySession), "T Test")
  expect_equal(getUserPhone(shinySession), "04050607")
})


# Restore instance
Sys.setenv(R_RAP_INSTANCE=currentInstance)