#' Shiny modules and helper functions for registry usage reports
#'
#' These modules may be used by registries for easy setup of usage reports.
#' The intended purpose is to provide registry staff access to when and by
#' whom the resources at Rapporteket were used, \emph{i.e.} application
#' start-up and single report usage. As such, this will be a tool to provide
#' useful statistics. However, it might also serve as a formal monitor utility
#' but only if logging is carefully implemented throughout the relevant
#' functions that make up the registry application at Rapporteket.
#'
#' @param id Character string shiny module id
#' @param registryName Character string registry name key
#' @param eligible Logical defining if the module should be allowed to work at
#' full capacity. This might be useful when access to module products should be
#' restricted. Default is TRUE, \emph{i.e.} no restrictions.
#' @param log Data frame containing log data (in Rapporteket format)
#' @param startDate Date object defining start of interval (character
#' representation "YYYY-MM-DD")
#' @param endDate Date object defining end of interval (character representation
#' "YYYY-MM-DD")
#'
#' @return Shiny objects, mostly. Helper functions may return other stuff too.
#' @name stats
#' @aliases statsInput statsUI statsServer statsApp logFormat logTimeFrame
#'
#' @examples
#' # client user interface function
#' ui <- shiny::fluidPage(
#'   shiny::sidebarLayout(
#'     shiny::sidebarPanel(statsInput("test")),
#'     shiny::mainPanel(statsUI("test"))
#'   )
#' )
#'
#' # server function
#' server <- function(input, output, session) {
#'   statsServer("test", registryName = "rapbase", eligible = TRUE)
#' }
#'
#' # run the shiny app in an interactive environment
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
NULL

#' @rdname stats
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

#' @rdname stats
#' @export
statsUI <- function(id) {
  shiny::tagList(
    rpivotTable::rpivotTableOutput(shiny::NS(id, "pivot"))
  )
}


#' @rdname stats
#' @export
statsServer <- function(id, registryName, eligible = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    log <- shiny::reactive({
      readLog(input$type, registryName) %>%
        logFormat()
    })

    logFrame <- shiny::reactive({
      shiny::req(input$period)
      logTimeFrame(log(), input$period[1], input$period[2])
    })

    output$period <- shiny::renderUI({
      shiny::dateRangeInput(
        shiny::NS(id, "period"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("calendar")), "Periode:")
        ),
        start = min(log()$date), end = max(log()$date), separator = "-"
      )
    })

    output$downloadButton <- shiny::renderUI({
      if (eligible) {
        shiny::downloadButton(shiny::NS(id, "download"), "Last ned!")
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
        rpivotTable::rpivotTable(logFrame(),
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

#' @rdname stats
#' @export
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


#' Shiny modules providing the Stats Guide
#'
#' @param id Character string module ID
#' @param registryName Character string registry name key
#'
#' @return Functions ui and server representing the (module) app
#' @name statsGuide
#' @aliases statsGuideUI statsGuideServer statsGuideApp
#' @examples
#' ui <- shiny::fluidPage(
#'   statsGuideUI("statsGuide")
#' )
#'
#' server <- function(input, output, session) {
#'   statsGuideServer("statsGuide", "test")
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
NULL

#' @rdname statsGuide
#' @export
statsGuideUI <- function(id) {
  shiny::htmlOutput(shiny::NS(id, "statsGuide"))
}

#' @rdname statsGuide
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

#' @rdname statsGuide
#' @export
statsGuideApp <- function() {
  ui <- shiny::fluidPage(
    statsGuideUI("statsGuide")
  )

  server <- function(input, output, session) {
    statsGuideServer("statsGuide", "test")
  }

  shiny::shinyApp(ui, server)
}


#' @rdname stats
#' @export
logFormat <- function(log) {
  log <- log %>%
    dplyr::mutate(
      datetime = as.POSIXct(.data$time),
      date = as.Date(.data$datetime),
      time = format(.data$datetime, "%H:%M:%S"),
      year = as.POSIXlt(.data$datetime)$year + 1900,
      month = as.POSIXlt(.data$datetime)$mon + 1,
      day = as.POSIXlt(.data$datetime)$mday,
      weekday = ifelse(as.POSIXlt(.data$datetime)$wday == 0, 7,
        as.POSIXlt(.data$datetime)$wday
      )
    )

  invisible(log)
}

#' @rdname stats
#' @export
logTimeFrame <- function(log, startDate, endDate) {
  dplyr::filter(log, date >= startDate & date <= endDate)
}
