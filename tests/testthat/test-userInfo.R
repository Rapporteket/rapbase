context("User info")


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
