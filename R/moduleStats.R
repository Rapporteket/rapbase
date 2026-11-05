#' Stats Input UI
#'
#' User interface module for selecting stats type, time period and download
#' format.
#'
#' @param id Character string providing the Shiny module id.
#'
#' @return A Shiny UI object.
#'
#' @export
statsInput <- function(id) {
  shiny::tagList(
    shiny::radioButtons(
      shiny::NS(id, "type"),
      label = shiny::tags$div(
        shiny::HTML(as.character(shiny::icon("shapes")), "Kategori:")
      ),
      choices = list(Innlogging = "app", Enkeltrapporter = "report")
    ),
    shiny::uiOutput(shiny::NS(id, "period")),
    shiny::radioButtons(
      shiny::NS(id, "downloadFormat"),
      label = shiny::tags$div(
        shiny::HTML(as.character(shiny::icon("file-csv")), "Nedlastingsformat:")
      ),
      choices = c("csv", "xlsx-csv")
    ),
    shiny::uiOutput(shiny::NS(id, "downloadButton")),
    shiny::uiOutput(shiny::NS(id, "eligible"))
  )
}

#' Stats Output UI
#'
#' User interface module for rendering the usage statistics pivot table.
#'
#' @param id Character string providing the Shiny module id.
#'
#' @return A Shiny UI object.
#'
#' @export
statsUI <- function(id) {
  shiny::tagList(
    rpivotTable::rpivotTableOutput(shiny::NS(id, "pivot"))
  )
}


#' Stats Server Module
#'
#' Server module for creating usage statistics based on log data.
#'
#' @param id Character string providing the Shiny module id.
#' @param registryName Character string with registry package name.
#' @param app_id Optional identifier used to filter log entries for a specific
#'   application.
#' @param eligible Logical indicating whether the module should be available at
#'   full capacity. Default is TRUE.
#'
#' @return A Shiny server module.
#'
#' @export
statsServer <- function(id,
                        registryName,
                        app_id = NULL,
                        eligible = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    log <- shiny::reactive({
      readLog(input$type, registryName, app_id) |>
        logFormat()
    })

    logFrame <- shiny::reactive({
      shiny::req(input$period)
      logTimeFrame(log(), input$period[1], input$period[2])
    })

    output$period <- shiny::renderUI({
      shiny::dateRangeInput(
        session$ns("period"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("calendar")), "Periode:")
        ),
        start = min(c(log()$date, Sys.Date() - 31)),
        end = max(c(log()$date, Sys.Date() - 30)),
        separator = "-",
        weekstart = 1,
        language = "no"
      )
    })

    output$downloadButton <- shiny::renderUI({
      if (eligible) {
        shiny::downloadButton(session$ns("download"), "Last ned!")
      } else {
        NULL
      }
    })

    if (eligible) {
      output$download <- shiny::downloadHandler(
        filename = function() {
          basename(
            tempfile(
              pattern = paste0("useStats-", registryName),
              fileext = ".csv"
            )
          )
        },
        content = function(file) {
          if (input$downloadFormat == "xlsx-csv") {
            readr::write_excel_csv2(logFrame(), file)
          } else {
            readr::write_csv2(logFrame(), file)
          }
        }
      )
    }

    output$eligible <- shiny::renderUI({
      if (eligible) {
        NULL
      } else {
        shiny::tagList(
          shiny::h4("Funksjonen er ikke tilgjengelig"),
          shiny::p("Ta kontakt med registeret")
        )
      }
    })
    output$pivot <- rpivotTable::renderRpivotTable(
      if (eligible) {
        rpivotTable::rpivotTable(
          logFrame(),
          rows = c("name"),
          cols = c("year", "month"),
          rendererName = "Heatmap"
        )
      } else {
        rpivotTable::rpivotTable(data.frame())
      }
    )
  })
}

#' Stats Server Module with Reactive Eligibility
#'
#' Server module for creating usage statistics with eligibility controlled by a
#' reactive value.
#'
#' @param id Character string providing the Shiny module id.
#' @param registryName Character string with registry package name.
#' @param app_id Optional identifier used to filter log entries for a specific
#'   application.
#' @param eligible Reactive logical indicating whether the module should be
#'   available at full capacity. Default is shiny::reactiveVal(TRUE).
#'
#' @return A Shiny server module.
#'
#' @export
statsServer2 <- function(id,
                         registryName,
                         app_id = NULL,
                         eligible = shiny::reactiveVal(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    log <- shiny::reactive({
      readLog(input$type, registryName, app_id) |>
        logFormat()
    })

    logFrame <- shiny::reactive({
      shiny::req(input$period)
      logTimeFrame(log(), input$period[1], input$period[2])
    })

    output$period <- shiny::renderUI({
      shiny::dateRangeInput(
        session$ns("period"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("calendar")), "Periode:")
        ),
        start = min(log()$date), end = max(log()$date), separator = "-",
        weekstart = 1,
        language = "no"
      )
    })

    output$downloadButton <- shiny::renderUI({
      if (eligible()) {
        shiny::downloadButton(session$ns("download"), "Last ned!")
      } else {
        NULL
      }
    })

    shiny::observeEvent(eligible(), {
      if (eligible()) {
        output$download <- shiny::downloadHandler(
          filename = function() {
            basename(
              tempfile(
                pattern = paste0("useStats-", registryName),
                fileext = ".csv"
              )
            )
          },
          content = function(file) {
            if (input$downloadFormat == "xlsx-csv") {
              readr::write_excel_csv2(logFrame(), file)
            } else {
              readr::write_csv2(logFrame(), file)
            }
          }
        )
      }
    }
    )

    output$eligible <- shiny::renderUI({
      if (eligible()) {
        NULL
      } else {
        shiny::tagList(
          shiny::h4("Funksjonen er ikke tilgjengelig"),
          shiny::p("Ta kontakt med registeret")
        )
      }
    })
    output$pivot <- rpivotTable::renderRpivotTable(
      if (eligible()) {
        rpivotTable::rpivotTable(
          logFrame(),
          rows = c("name"),
          cols = c("year", "month"),
          rendererName = "Heatmap"
        )
      } else {
        rpivotTable::rpivotTable(data.frame())
      }
    )
  })
}

#' Stats App for Testing
#'
#' Launch a small Shiny app that demonstrates the stats module.
#'
#' @return A Shiny application object.
#'
#' @keywords internal
statsApp <- function() {
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(statsInput("test")),
      shiny::mainPanel(statsUI("test"))
    )
  )

  server <- function(input, output, session) {
    statsServer("test", registryName = "rapbase")
  }

  shiny::shinyApp(ui, server)
}


#' Stats Guide UI
#'
#' User interface module for displaying the stats guide.
#'
#' @param id Character string providing the Shiny module id.
#'
#' @return A Shiny UI object.
#'
#' @export
statsGuideUI <- function(id) {
  shiny::htmlOutput(shiny::NS(id, "statsGuide"))
}

#' Stats Guide Server Module
#'
#' Server module for rendering the stats guide text.
#'
#' @param id Character string providing the Shiny module id.
#' @param registryName Character string with registry package name.
#'
#' @return A Shiny server module.
#'
#' @export
statsGuideServer <- function(id, registryName) {
  shiny::moduleServer(id, function(input, output, session) {
    output$statsGuide <- shiny::renderUI({
      renderRmd(
        sourceFile = system.file("statsGuide.Rmd", package = "rapbase"),
        outputType = "html_fragment",
        params = list(registryName = registryName)
      )
    })
  })
}

#' Stats Guide App for Testing
#'
#' Launch a Shiny app that demonstrates the stats guide module.
#'
#' @return A Shiny application object.
#'
#' @keywords internal
statsGuideApp <- function() {
  ui <- shiny::fluidPage(
    statsGuideUI("statsGuide")
  )

  server <- function(input, output, session) {
    statsGuideServer("statsGuide", "test")
  }

  shiny::shinyApp(ui, server)
}


#' Format Log Data
#'
#' Convert a raw log data frame into a format suitable for stats analysis.
#'
#' @param log Data frame containing log data in Rapporteket format.
#'
#' @return A data frame with formatted date and time columns.
#'
#' @keywords internal
logFormat <- function(log) {
  log <- log |>
    dplyr::mutate(
      datetime = as.POSIXct(.data$time),
      date = as.Date(.data$datetime),
      time = format(.data$datetime, "%H:%M:%S"),
      year = as.POSIXlt(.data$datetime)$year + 1900,
      month = as.POSIXlt(.data$datetime)$mon + 1,
      day = as.POSIXlt(.data$datetime)$mday,
      weekday = ifelse(
        as.POSIXlt(.data$datetime)$wday == 0, 7,
        as.POSIXlt(.data$datetime)$wday
      )
    )

  invisible(log)
}

#' Filter Log by Date Interval
#'
#' Keep only log entries between the supplied start and end dates.
#'
#' @param log Data frame containing log data in Rapporteket format.
#' @param startDate Date object defining the start of the interval.
#' @param endDate Date object defining the end of the interval.
#'
#' @return A filtered data frame with rows inside the interval.
#'
#' @keywords internal
logTimeFrame <- function(log, startDate, endDate) {
  dplyr::filter(log, date >= startDate & date <= endDate)
}
