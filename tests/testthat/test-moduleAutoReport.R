## store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

# make pristine and dedicated config to avoid interference with other tests
Sys.setenv(R_RAP_CONFIG_PATH = file.path(tempdir(), "autoReportTesting"))
dir.create(Sys.getenv("R_RAP_CONFIG_PATH"))
file.copy(
  system.file(c("rapbaseConfig.yml", "dbConfig.yml", "autoReport.yml"),
    package = "rapbase"
  ),
  Sys.getenv("R_RAP_CONFIG_PATH")
)

registryName <- "rapbase"

test_that("auto report UI returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(autoReportUI("id")))
})

test_that("auto report Org input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(autoReportOrgInput("id")))
})

test_that("auto report Format input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(autoReportFormatInput("id")))
})

test_that("auto report input returns a shiny tag list", {
  expect_true("shiny.tag.list" %in% class(autoReportInput("id")))
})


# prep arguments
## make a list for report metadata
reports <- list(
  FirstReport = list(
    synopsis = "First example report",
    fun = "fun1",
    paramNames = c("organization", "outputFormat"),
    paramValues = c(111111, "html")
  ),
  SecondReport = list(
    synopsis = "Second example report",
    fun = "fun2",
    paramNames = c("organization", "outputFormat"),
    paramValues = c(111111, "pdf")
  )
)

## make a list of organization names and numbers
orgs <- list(
  OrgOne = 111111,
  OrgTwo = 222222
)

test_that("module org server returns outputs and list of reactives", {
  shiny::testServer(autoReportOrgServer, args = list(orgs = orgs), {
    session$setInputs(org = 111111)
    expect_equal(class(output$orgs), "list")
    expect_equal(class(session$returned), "list")
    expect_true(shiny::is.reactive(session$returned$name))
    expect_true(shiny::is.reactive(session$returned$value))
  })
})

test_that("module format server returns outputs and list of reactives", {
  shiny::testServer(autoReportFormatServer, {
    expect_equal(class(output$format), "list")
    expect_true(shiny::is.reactive(session$returned))
  })
})

## set type
type <- "subscription"

test_that("module server provides sensible output", {
  shiny::testServer(autoReportServer,
    args = list(
      registryName = registryName, type = type,
      reports = reports, orgs = orgs
    ),
    {
      session$setInputs(report = "FirstReport")
      expect_equal(class(output$reports), "list")
    }
  )
})

test_that("no report select list created when no reports available", {
  shiny::testServer(
    autoReportServer,
    args = list(
      registryName = registryName, type = type,
      reports = NULL, orgs = orgs
    ),
    {
      expect_true(is.null(output$reports))
    }
  )
})

type <- "dispatchment"
test_that("email can be added and deleted for dispatchment", {
  shiny::testServer(
    autoReportServer,
    args = list(
      registryName = registryName, type = type,
      org = shiny::reactive(111111),
      reports = reports, orgs = orgs
    ),
    {
      session$setInputs(email = "true@email.no")
      expect_equal(length(autoReport$email), 0)
      session$setInputs(addEmail = 1)
      expect_equal(autoReport$email[1], "true@email.no")
      session$setInputs(delEmail = 1)
      expect_equal(length(autoReport$email), 0)
    }
  )
})


test_that("new dispatchment can be written to and removed from file", {
  origFileSize <- file.size(file.path(
    Sys.getenv("R_RAP_CONFIG_PATH"),
    "autoReport.yml"
  ))
  shiny::testServer(
    autoReportServer,
    args = list(
      registryName = registryName, type = type,
      org = shiny::reactive(111111),
      reports = reports, orgs = orgs, freq = "year"
    ),
    {
      session$setInputs(report = "FirstReport")
      session$setInputs(freq = "Aarlig-year")
      session$setInputs(start = as.character(Sys.Date()))
      session$setInputs(email = "true@email.no")
      session$setInputs(addEmail = 1)
      session$setInputs(makeAutoReport = 1)
      expect_true(origFileSize < file.size(
        file.path(
          Sys.getenv("R_RAP_CONFIG_PATH"),
          "autoReport.yml"
        )
      ))
      # get newly created edit button id (from last entry in table)
      # and test it by entry being removed from table
      btnRaw <- autoReport$tab[dim(autoReport$tab)[1], ]$Endre
      editButton <- rvest::read_html(btnRaw) %>%
        rvest::html_element("button") %>%
        rvest::html_attr("id")
      repsBefore <- dim(autoReport$tab)[1]
      session$setInputs(edit_button = editButton)
      repsAfter <- dim(autoReport$tab)[1]
      expect_true(repsAfter == (repsBefore - 1))
      # then, true deletion (after adding one more time)
      session$setInputs(makeAutoReport = 2)
      expect_true(repsBefore == dim(autoReport$tab)[1])
      btnRaw <- autoReport$tab[dim(autoReport$tab)[1], ]$Slett
      delButton <- rvest::read_html(btnRaw) %>%
        rvest::html_element("button") %>%
        rvest::html_attr("id")
      session$setInputs(del_button = delButton)
      repsAfter <- dim(autoReport$tab)[1]
      expect_true(repsAfter == (repsBefore - 1))
    }
  )
})

test_that("paramValues can be tweaked when provided", {
  origFileSize <- file.size(file.path(
    Sys.getenv("R_RAP_CONFIG_PATH"),
    "autoReport.yml"
  ))
  shiny::testServer(
    autoReportServer,
    args = list(
      registryName = registryName, type = type,
      org = shiny::reactive(111111),
      paramNames = shiny::reactive(c("organization", "outputFormat")),
      paramValues = shiny::reactive(c(999999, "pdf")),
      reports = reports, orgs = orgs
    ),
    {
      session$setInputs(report = "FirstReport")
      session$setInputs(freq = "Maanedlig-month")
      session$setInputs(start = as.character(Sys.Date()))
      session$setInputs(email = "true@email.no")
      session$setInputs(addEmail = 1)
      session$setInputs(makeAutoReport = 1)
      expect_true(origFileSize < file.size(
        file.path(
          Sys.getenv("R_RAP_CONFIG_PATH"),
          "autoReport.yml"
        )
      ))
    }
  )
})

test_that("new subscription can be written to and removed from file", {
  origFileSize <- file.size(file.path(
    Sys.getenv("R_RAP_CONFIG_PATH"),
    "autoReport.yml"
  ))
  shiny::testServer(
    autoReportServer,
    args = list(
      registryName = registryName, type = "subscription",
      reports = reports, orgs = orgs
    ),
    {
      session$setInputs(report = "FirstReport")
      session$setInputs(freq = "Maanedlig-month")
      session$setInputs(start = as.character(Sys.Date()))
      session$setInputs(makeAutoReport = 1)
      expect_true(origFileSize < file.size(
        file.path(
          Sys.getenv("R_RAP_CONFIG_PATH"),
          "autoReport.yml"
        )
      ))
      # get newly created edit button id (from last entry in table)
      # and test it by entry being removed from table
      btnRaw <- autoReport$tab[dim(autoReport$tab)[1], ]$Endre
      editButton <- rvest::read_html(btnRaw) %>%
        rvest::html_element("button") %>%
        rvest::html_attr("id")
      repsBefore <- dim(autoReport$tab)[1]
      session$setInputs(edit_button = editButton)
      repsAfter <- dim(autoReport$tab)[1]
      expect_true(repsAfter == (repsBefore - 1))
      # then, true deletion (after adding one more time)
      session$setInputs(makeAutoReport = 2)
      expect_true(repsBefore == dim(autoReport$tab)[1])
      btnRaw <- autoReport$tab[dim(autoReport$tab)[1], ]$Slett
      delButton <- rvest::read_html(btnRaw) %>%
        rvest::html_element("button") %>%
        rvest::html_attr("id")
      session$setInputs(del_button = delButton)
      repsAfter <- dim(autoReport$tab)[1]
      expect_true(repsAfter == (repsBefore - 1))
    }
  )
})

test_that("add email button is not created if email is not valid", {
  shiny::testServer(
    autoReportServer,
    args = list(
      registryName = registryName, type = type,
      org = shiny::reactive(111111),
      reports = reports, orgs = orgs
    ),
    {
      session$setInputs(email = "invalid@email-format")
      expect_true(is.null(output$editEmail))
      session$setInputs(email = "invalid@email-format.o")
      expect_true(is.null(output$editEmail))
      session$setInputs(email = "invalid.email-format.on")
      expect_true(is.null(output$editEmail))
    }
  )
})

test_that("no submit button is provided when module is not eligible", {
  shiny::testServer(
    autoReportServer,
    args = list(
      registryName = registryName, type = "subscription",
      reports = reports, orgs = orgs, eligible = FALSE
    ),
    {
      session$setInputs(email = "valid.email@format.no")
      expect_true(is.null(output$makeAutoReport))
    }
  )
})

test_that("tabel is replaced by message when no reports listed", {
  file.remove(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "autoReport.yml"))
  file.create(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "autoReport.yml"))
  shiny::testServer(
    autoReportServer,
    args = list(
      registryName = registryName, type = type,
      org = shiny::reactive(111111),
      reports = reports, orgs = orgs
    ),
    {
      session$flushReact()
      expect_true(dim(autoReport$tab)[1] == 0)
    }
  )
})

test_that("test app returns an app object", {
  expect_equal(class(autoReportApp()), "shiny.appobj")
})

## Duplicate for shinyproxy container instance
file.copy(
  system.file(
    c("autoReport.yml", "extdata/accesstree.json"),
    package = "rapbase"
  ),
  Sys.getenv("R_RAP_CONFIG_PATH")
)
with_envvar(
  new = c(
    "R_RAP_INSTANCE" = "QAC",
    "SHINYPROXY_USERNAME" = "ttesterc",
    "SHINYPROXY_USERGROUPS" = "rapbase,rapbase,utils,utils",
    "USERORGID" = "[1, 2, 3, 4]",
    "USERFULLNAME" = "Tore Tester Container",
    "USEREMAIL" = "ttesterc@rapporteket.no"
  ),
  code = {
    ## make a list for report metadata
    reports <- list(
      FirstReport = list(
        synopsis = "First example report",
        fun = "fun1",
        paramNames = c("organization", "outputFormat"),
        paramValues = c(100082, "html")
      ),
      SecondReport = list(
        synopsis = "Second example report",
        fun = "fun2",
        paramNames = c("organization", "outputFormat"),
        paramValues = c(102966, "pdf")
      )
    )
    ## make a list of organization names and numbers
    orgs <- list(
      OrgOne = 100082,
      OrgTwo = 102966
    )
    type <- "subscription"
    user <- userAttribute("rapbase", unit = 1)
    for (n in names(user)) {
      user[[n]] <- shiny::reactiveVal(user[[n]])
    }

    test_that("module server provides sensible output", {
      shiny::testServer(autoReportServer2,
                        args = list(
                          registryName = registryName, type = type,
                          reports = reports, orgs = orgs, user = user
                        ),
                        {
                          session$setInputs(report = "FirstReport")
                          expect_equal(class(output$reports), "list")
                        }
      )
    })

    test_that("no report select list created when no reports available", {
      shiny::testServer(
        autoReportServer2,
        args = list(
          registryName = registryName, type = type,
          reports = NULL, orgs = orgs, user = user
        ),
        {
          expect_true(is.null(output$reports))
        }
      )
    })

    type <- "dispatchment"
    test_that("email can be added and deleted for dispatchment", {
      shiny::testServer(
        autoReportServer2,
        args = list(
          registryName = registryName, type = type,
          org = shiny::reactive(100082),
          reports = reports, orgs = orgs, user = user
        ),
        {
          session$setInputs(email = "true@email.no")
          expect_equal(length(autoReport$email), 0)
          session$setInputs(addEmail = 1)
          expect_equal(autoReport$email[1], "true@email.no")
          session$setInputs(delEmail = 1)
          expect_equal(length(autoReport$email), 0)
        }
      )
    })


    test_that("new dispatchment can be written to and removed from file", {
      origFileSize <- file.size(file.path(
        Sys.getenv("R_RAP_CONFIG_PATH"),
        "autoReport.yml"
      ))
      shiny::testServer(
        autoReportServer2,
        args = list(
          registryName = registryName, type = type,
          org = shiny::reactive(100082),
          reports = reports, orgs = orgs, freq = "year", user = user
        ),
        {
          user$role <- shiny::reactive("SC")
          session$flushReact()
          print(autoReport$tab)
          session$setInputs(report = "FirstReport")
          session$setInputs(freq = "Aarlig-year")
          session$setInputs(start = as.character(Sys.Date()))
          session$setInputs(email = "true@email.no")
          session$setInputs(addEmail = 1)
          session$setInputs(makeAutoReport = 1)
          expect_true(origFileSize < file.size(
            file.path(
              Sys.getenv("R_RAP_CONFIG_PATH"),
              "autoReport.yml"
            )
          ))

          # get newly created edit button id (from last entry in table)
          # and test it by entry being removed from table
          btnRaw <- autoReport$tab[dim(autoReport$tab)[1], ]$Endre
          editButton <- rvest::read_html(btnRaw) %>%
            rvest::html_element("button") %>%
            rvest::html_attr("id")
          repsBefore <- dim(autoReport$tab)[1]
          session$setInputs(edit_button = editButton)
          repsAfter <- dim(autoReport$tab)[1]
          expect_true(repsAfter == (repsBefore - 1))
          # then, true deletion (after adding one more time)
          session$setInputs(makeAutoReport = 2)
          expect_true(repsBefore == dim(autoReport$tab)[1])
          btnRaw <- autoReport$tab[dim(autoReport$tab)[1], ]$Slett
          delButton <- rvest::read_html(btnRaw) %>%
            rvest::html_element("button") %>%
            rvest::html_attr("id")
          session$setInputs(del_button = delButton)
          repsAfter <- dim(autoReport$tab)[1]
          expect_true(repsAfter == (repsBefore - 1))
        }
      )
    })

    test_that("paramValues can be tweaked when provided", {
      origFileSize <- file.size(file.path(
        Sys.getenv("R_RAP_CONFIG_PATH"),
        "autoReport.yml"
      ))
      shiny::testServer(
        autoReportServer2,
        args = list(
          registryName = registryName, type = type,
          org = shiny::reactive(100082),
          paramNames = shiny::reactive(c("organization", "outputFormat")),
          paramValues = shiny::reactive(c(999999, "pdf")),
          reports = reports, orgs = orgs, user = user
        ),
        {
          session$setInputs(report = "FirstReport")
          session$setInputs(freq = "Maanedlig-month")
          session$setInputs(start = as.character(Sys.Date()))
          session$setInputs(email = "true@email.no")
          session$setInputs(addEmail = 1)
          session$setInputs(makeAutoReport = 1)
          expect_true(origFileSize < file.size(
            file.path(
              Sys.getenv("R_RAP_CONFIG_PATH"),
              "autoReport.yml"
            )
          ))
        }
      )
    })

    test_that("new subscription can be written to and removed from file", {
      origFileSize <- file.size(file.path(
        Sys.getenv("R_RAP_CONFIG_PATH"),
        "autoReport.yml"
      ))
      shiny::testServer(
        autoReportServer2,
        args = list(
          registryName = registryName, type = "subscription",
          org = shiny::reactive(100082),
          reports = reports, orgs = orgs, user = user
        ),
        {
          session$setInputs(report = "FirstReport")
          session$setInputs(freq = "Maanedlig-month")
          session$setInputs(start = as.character(Sys.Date()))
          session$setInputs(makeAutoReport = 1)
          expect_true(origFileSize < file.size(
            file.path(
              Sys.getenv("R_RAP_CONFIG_PATH"),
              "autoReport.yml"
            )
          ))
          # get newly created edit button id (from last entry in table)
          # and test it by entry being removed from table
          btnRaw <- autoReport$tab[dim(autoReport$tab)[1], ]$Endre
          editButton <- rvest::read_html(btnRaw) %>%
            rvest::html_element("button") %>%
            rvest::html_attr("id")
          repsBefore <- dim(autoReport$tab)[1]
          session$setInputs(edit_button = editButton)
          repsAfter <- dim(autoReport$tab)[1]
          expect_true(repsAfter == (repsBefore - 1))
          # then, true deletion (after adding one more time)
          session$setInputs(makeAutoReport = 2)
          expect_true(repsBefore == dim(autoReport$tab)[1])
          btnRaw <- autoReport$tab[dim(autoReport$tab)[1], ]$Slett
          delButton <- rvest::read_html(btnRaw) %>%
            rvest::html_element("button") %>%
            rvest::html_attr("id")
          session$setInputs(del_button = delButton)
          repsAfter <- dim(autoReport$tab)[1]
          expect_true(repsAfter == (repsBefore - 1))
        }
      )
    })

    test_that("add email button is not created if email is not valid", {
      shiny::testServer(
        autoReportServer2,
        args = list(
          registryName = registryName, type = type,
          org = shiny::reactive(100082),
          reports = reports, orgs = orgs, user = user
        ),
        {
          session$setInputs(email = "invalid@email-format")
          expect_true(is.null(output$editEmail))
          session$setInputs(email = "invalid@email-format.o")
          expect_true(is.null(output$editEmail))
          session$setInputs(email = "invalid.email-format.on")
          expect_true(is.null(output$editEmail))
        }
      )
    })

    test_that("no submit button is provided when module is not eligible", {
      shiny::testServer(
        autoReportServer2,
        args = list(
          registryName = registryName, type = "subscription",
          reports = reports, orgs = orgs, eligible = FALSE, user = user
        ),
        {
          session$setInputs(email = "valid.email@format.no")
          expect_true(is.null(output$makeAutoReport))
        }
      )
    })

    test_that("tabel is replaced by message when no reports listed", {
      file.remove(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "autoReport.yml"))
      file.create(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "autoReport.yml"))
      shiny::testServer(
        autoReportServer2,
        args = list(
          registryName = registryName, type = type,
          org = shiny::reactive(100082),
          reports = reports, orgs = orgs, user = user
        ),
        {
          session$flushReact()
          expect_true(dim(autoReport$tab)[1] == 0)
        }
      )
    })
  }
)

# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
