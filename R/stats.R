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
#' @param type Character string defining data level. One of
#' \code{c("app", "report")}.
#' @param log Data frame containing log data (in Rapporteket format)
#' @param startDate Date object defining start of interval (character
#' representation "YYYY-MM-DD")
#' @param endDate Date object defining end of interval (character representation
#' "YYYY-MM-DD")
#'
#' @return Shiny objects, mostly. Helper functions may return other stuff too.
#' @name stats
#' @aliases statsInput statsUI statsServer statsApp getRegistryLog logFormat
#' logTimeFrame
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
#'   statsServer("test", registryName = "rapbase")
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
      shiny::NS(id, "type"), "Kategori:",
      choices = list(Innlogging = "app", Enkeltrapporter = "report")
    ),
    shiny::uiOutput(shiny::NS(id, "period")),
    shiny::radioButtons(
      shiny::NS(id, "downloadFormat"), "Nedlastingsformat:",
      choices = c("csv", "xlsx-csv")
    ),
    shiny::downloadButton(shiny::NS(id, "download"), "Last ned!")
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
statsServer <- function(id, registryName) {

  shiny::moduleServer(id, function(input, output, session) {

    log <- shiny::reactive({
      getRegistryLog(registryName, input$type) %>%
        logFormat()
    })

    logFrame <- shiny::reactive({
      shiny::req(input$period)
      logTimeFrame(log(), input$period[1], input$period[2])
    })

    output$period <- shiny::renderUI({
      shiny::dateRangeInput(
        shiny::NS(id, "period"), "Periode:",
        start = min(log()$date), end = max(log()$date), separator = "-"
      )
    })
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
    output$pivot <- rpivotTable::renderRpivotTable(
      rpivotTable::rpivotTable(logFrame(),
                               rows = c("name"),
                               cols = c("year", "month"),
                               rendererName = "Heatmap")
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


#' @rdname stats
#' @export
getRegistryLog <- function(registryName, type = "app") {

  stopifnot(type == "report" | type == "app")
  path <- Sys.getenv("R_RAP_CONFIG_PATH")

  if (path == "") {
    stop(paste("No path to log-files provided. Make sure the environment",
               "varaible R_RAP_CONFIG_PATH is set!"))
  }

  logFile <- switch(type,
                    "report" = file.path(path, "reportLog.csv"),
                    "app" = file.path(path, "appLog.csv")
  )
  if (!file.exists(logFile)) {
    stop(paste("Cannot find the log!", logFile, "does not exist."))
  }

  log <- utils::read.csv(logFile,
                         header = TRUE,
                         stringsAsFactors = FALSE)
  invisible(log)

}

#' @rdname stats
#' @export
logFormat <- function(log) {

  log <- log %>%
    dplyr::mutate(
      datetime = as.POSIXct(.data$time),
      date = as.Date(.data$datetime),
      time = format(.data$datetime, "%H:%M:%S"),
      year = lubridate::year(.data$datetime),
      month = lubridate::month(.data$datetime),
      day = lubridate::day(.data$datetime),
      weekday = lubridate::wday(
        .data$datetime,
        week_start = 1,
        abbr = FALSE)
    )

  invisible(log)
}

#' @rdname stats
#' @export
logTimeFrame <- function(log, startDate, endDate) {

  dplyr::filter(log, date >= startDate & date <= endDate)

}