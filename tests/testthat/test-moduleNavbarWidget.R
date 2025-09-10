
registryName <- "rapbase"

test_that("navbar widget input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(navbarWidgetInput("id")))
})


test_that("test app returns an app object", {
  expect_equal(class(navbarWidgetApp()), "shiny.appobj")
})


## new widget for shinyproxy container instances
with_envvar(
  new = c(
    "FALK_EXTENDED_USER_RIGHTS" = "[{\"A\":80,\"R\":\"LU\",\"U\":1},{\"A\":80,\"R\":\"SC\",\"U\":2},{\"A\":81,\"R\":\"LC\",\"U\":2}]",
    "FALK_APP_ID" = "80",
    "FALK_USER_FULLNAME" = "Tore Tester Container",
    "R_RAP_INSTANCE" = "QAC",
    "SHINYPROXY_USERNAME" = "ttesterc",
    "SHINYPROXY_USERGROUPS" = "rapbase,rapbase,utils"
  ),
  code = {

    test_that("shinyproxy-like module navbar widget server returns output", {
      shiny::testServer(navbarWidgetServer2, args = list(
        orgName = registryName,
        caller = "rapbase"
      ), {
        session$setInputs(unit = NULL)
        expect_equal(class(output$affiliation), "character")
        expect_equal(rv$name, "ttesterc")
        expect_equal(rv$fullName, "Tore Tester Container")
        expect_equal(rv$group, 80)
        expect_equal(rv$unit, 1)
        expect_equal(rv$org, 1)
        expect_equal(rv$role, "LU")

        session$setInputs(selectOrganization = 3,
                          unit = paste0("Ukjent", " (", "2", ") - ", "SC"))
        expect_equal(rv$unit, 2)
        expect_equal(rv$org, 2)
        expect_equal(rv$role, "SC")
        session$setInputs(unit = NULL)
        expect_equal(rv$unit, 2)
        expect_equal(rv$org, 2)
        expect_equal(rv$role, "SC")
        session$setInputs(selectOrganization = 4,
                          unit = NULL)
        expect_equal(rv$unit, 2)
        expect_equal(rv$org, 2)
        expect_equal(rv$role, "SC")
      })
    })
  }
)

