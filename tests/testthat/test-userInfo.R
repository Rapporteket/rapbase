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

# New: container instance for QA and PRODUCTION contexts
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
file.copy(
  system.file(
    c("rapbaseConfig.yml"), package = "rapbase"
  ),
  Sys.getenv("R_RAP_CONFIG_PATH")
)


withr::with_envvar(
  new = c(
    "FALK_EXTENDED_USER_RIGHTS" = "rapbase",
    "FALK_APP_ID" = ""
  ),
  code = {
    test_that("errors are returned when insufficient system environment", {
      expect_error(
        userAttribute(),
        regexp = "Environmental variables FALK_EXTENDED_USER_RIGHTS and FALK_APP_ID"
      )
    })
  }
)

withr::with_envvar(
  new = c(
    "FALK_EXTENDED_USER_RIGHTS" = "",
    "FALK_APP_ID" = "1"
  ),
  code = {
    test_that("errors are returned when insufficient system environment", {
      expect_error(
        userAttribute(),
        regexp = "Environmental variables FALK_EXTENDED_USER_RIGHTS and FALK_APP_ID"
      )
    })
  }
)

withr::with_envvar(
  new = c(
    "FALK_EXTENDED_USER_RIGHTS" = "[{\"A\":80,\"R\":\"LC\",\"U\":1},{\"A\":80,\"R\":\"SC\",\"U\":2},{\"A\":81,\"R\":\"LC\",\"U\":2}]",
    "FALK_APP_ID" = "80"
  ),
  code = {
    test_that("group and unit are returned correspondingly when unit = NULL", {
      expect_true(class(userAttribute()) == "list")
      expect_true(
        length(userAttribute()$group) ==
          length(userAttribute()$unit)
      )
      expect_true(length(userAttribute()$group) == 2)
      expect_true(userAttribute()$group[1] == "80")
      expect_true(userAttribute()$group[2] == "80")
      expect_true(userAttribute()$unit[1] == "1")
      expect_true(userAttribute()$unit[2] == "2")
    })

    test_that("group and unit returned correspondingly when unit is given", {
      expect_equal(class(userAttribute()), "list")
      expect_equal(
        length(userAttribute(unit = 2)$group),
        length(userAttribute(unit = 2)$unit)
      )
      expect_equal(
        userAttribute(unit = 2)$unit, 2
      )
      expect_equal(userAttribute(unit = 3)$unit, integer(0))
    })

    test_that("correct lookup values are provided", {
      expect_equal(
        length(userAttribute(unit = 2)$group),
        length(userAttribute(unit = 2)$unit)
      )
      expect_equal(
        userAttribute(unit = 2)$unit, 2
      )
      expect_equal(userAttribute(unit = 3)$unit, integer(0))
    })

    test_that("orgname-mapping is working", {
      maporg <- data.frame(orgname = c("qwerty", "asdfgh"), UnitId = c(1, 2))
      expect_equal(
        userAttribute(unit = 2)$orgName, "Ukjent"
      )
      expect_equal(
        userAttribute(unit = 2, map_orgname = maporg)$orgName, "asdfgh"
      )
      expect_equal(
        userAttribute(unit = 1, map_orgname = maporg)$orgName, "qwerty"
      )
    })
  }
)

withr::with_envvar(
  new = c(
    "FALK_EXTENDED_USER_RIGHTS" = paste0(
      '[{\"A\":80,\"R\":\"SC\",\"U\":102966},',
      '{\"A\":80,\"R\":\"LU\",\"U\":4219765},',
      '{\"A\":80,\"R\":\"LC\",\"U\":4219765},',
      '{\"A\":80,\"R\":\"LC\",\"U\":700328}]"'
    ),
    "FALK_APP_ID" = "80",
    "R_RAP_INSTANCE" = "QAC",
    "SHINYPROXY_USERNAME" = "userc",
    "FALK_USER_EMAIL" = "userc@container.no",
    "FALK_USER_FULLNAME" = "User Container",
    "FALK_USER_PHONE" = "+4787654321"
  ),
  code = {
    test_that("User attribs can be fetched in container instance (QA, PROD)", {
      expect_equal(getUserName(shinySession, "rapbase"), "userc")
      expect_equal(getUserGroups(shinySession, "rapbase"), 80)
      expect_equal(getUserReshId(shinySession, "rapbase"), 102966)
      expect_equal(getUserRole(shinySession, "rapbase"), "SC")
      expect_equal(getUserEmail(shinySession, "rapbase"), "userc@container.no")
      expect_equal(getUserFullName(shinySession, "rapbase"), "User Container")
      expect_equal(getUserPhone(shinySession, "rapbase"), "+4787654321")
    })
  }
)

# Restore instance
Sys.setenv(R_RAP_INSTANCE = currentInstance)
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
