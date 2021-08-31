#' Shiny modules and helper functions for registry auto reports
#'
#' These shiny modules may be used to set up auto reporting from registries at
#' Rapporteket.
#'
#' The \emph{reports} argument must be a list where each entry
#' represents one report and its name will be used in the auto report user
#' interface for selecting reports, \emph{e.g.}
#' \code{reports = list(MagicReport = ...)} will produce the entry "MagicReport"
#' in the GUI selectable reports. The value of each entry must be another list
#' with the following names and values:
#' \describe{
#'   \item{synopsis}{character string describing the report}
#'   \item{fun}{report function base name (without"()")}
#'   \item{paramNames}{character vector naming all arguments of \emph{fun}}
#'   \item{paramValues}{vector with values corresponding to \emph{paramNames}}
#' }
#' These named values will be used to run reports none-interactively on a given
#' schedule and must therefore represent existing and exported functions from
#' the registry R package. For subscriptions the \emph{reports} list can be used
#' as is, more specifically that the values provided in \emph{paramValues} can
#' go unchanged. For dispatchments and bulletins it is likely that paramter
#' values must be set dynamically in which case \emph{paramValues} must be
#' a reactive part of the application. See Examples.
#'
#' @param id Character string providing the shiny module id.
#' @param registryName Character string with the registry name key. Must
#' correspond to the registry R package name.
#' @param type Character string defining the type of auto reports. Must be one
#' of \code{c("subscription", "dispatchment", "bulletin")}
#' @param reports List of a given structure that provides meta data for the
#' reports that are made available as automated reports. See Details for further
#' description.
#' @param orgs Named list of organizations (names) and ids (values). When set to
#' \code{NULL} (default) the ids found in auto report data will be used in the
#' table listing existing auto reports.
#'
#' @return In general, shiny objects. In particular, \code{autoreportServer}
#' returns a named list of "format" and "org" with reactive values providing the
#' selected file format and organization as these may be used when this module
#' is implemented  by the registries. \code{orgList2df} returns a data frame
#' with colums "name" and "id".
#' @name autoReport
#' @aliases autoReportUI autoReportOrgInput autoReportOrgServer
#' autoReportFormatInput autoReportFormatSercer autoReportInput autoReportServer
#' autoReportApp orgList2df
#' @examples
#' # make a list for report metadata
#' reports <- list(
#'   FirstReport = list(
#'     synopsis = "First example report",
#'     fun = "fun1",
#'     paramNames = c("a", "b"),
#'     paramValues = c(1, "yes")
#'   ),
#'   SecondReport = list(
#'     synopsis = "Second example report",
#'     fun = "fun2",
#'     paramNames = "x",
#'     paramValues = 0
#'   )
#' )
#'
#' # make a list of organization names and numbers
#' orgs <- list(
#'   OrgOne = 111111,
#'   OrgTwo = 222222
#' )
#'
#' # client user interface function
#' ui <- shiny::fluidPage(
#'   shiny::sidebarLayout(
#'     shiny::sidebarPanel(autoReportInput("test")),
#'     shiny::mainPanel(autoReportUI("test"))
#'   )
#' )
#'
#' # server function
#' server <- function(input, output, session) {
#'   autoReportServer(id = "test", registryName = "rapbase",
#'                    type = "subscription", reports = reports, orgs = orgs)
#' }
#'
#' # run the shiny app in an interactive environment
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
NULL

#' @rdname autoReport
#' @export
autoReportUI <- function(id) {

  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "autoReportTable"))
  )
}

#' @rdname autoReport
#' @export
autoReportOrgInput <- function(id) {

  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "orgs"))
  )
}

#' @rdname autoReport
#' @export
autoReportOrgServer <- function(id, orgs) {
  shiny::moduleServer(id, function(input, output, session) {

    output$orgs <- shiny::renderUI({
      shiny::selectInput(
        shiny::NS(id, "org"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("database")),
                      "Velg datakilde:")
        ),
        choices = orgs,
        selected = unlist(orgs, use.names = FALSE)[1]
      )
    })

    # return reactive of whatever selected
    list(
      name = shiny::reactive(
        names(orgs)[unlist(orgs, use.names = FALSE) == input$org]
      ),
      value = shiny::reactive(input$org)
    )
  })
}

#' @rdname autoReport
#' @export
autoReportFormatInput <- function(id) {

  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "format"))
  )
}

#' @rdname autoReport
#' @export
autoReportFormatServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    output$format <- shiny::renderUI({
      shiny::selectInput(
        shiny::NS(id, "format"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("file-pdf")), "Fil-format:")
        ),
        choices = c("html", "pdf")
      )
    })

    # return reactive of whatever selected
    shiny::reactive(input$format)
  })
}

#' @rdname autoReport
#' @export
autoReportInput <- function(id) {

  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "reports")),
    shiny::uiOutput(shiny::NS(id, "synopsis")),
    shiny::tags$hr(),
    shiny::uiOutput(shiny::NS(id, "freq")),
    shiny::uiOutput(shiny::NS(id, "start")),
    shiny::uiOutput(shiny::NS(id, "email")),
    shiny::htmlOutput(shiny::NS(id, "editEmail")),
    shiny::htmlOutput(shiny::NS(id, "recipient")),
    shiny::tags$hr(),
    shiny::uiOutput(shiny::NS(id, "makeAutoReport"))
  )
}

#' @rdname autoReport
#' @export
autoReportServer <- function(id, registryName, type, org, paramValues,
                             reports = NULL, orgs = NULL) {

  if (!type %in% c("subscription")) {
    stopifnot(shiny::is.reactive(org))
    stopifnot(shiny::is.reactive(paramValues))
  }

  shiny::moduleServer(id, function(input, output, session) {

    autoReport <- shiny::reactiveValues(
      tab = rapbase::makeAutoReportTab(session = session, namespace = id,
                                       type = type,
                                       mapOrgId = orgList2df(orgs)),
      report = names(reports)[1],
      org = unlist(orgs, use.names = FALSE)[1],
      freq = "M\u00E5nedlig-month",
      email = vector()
    )

    shiny::observeEvent(input$addEmail, {
      autoReport$email <- c(autoReport$email, input$email)
    })

    shiny::observeEvent(input$delEmail, {
      autoReport$email <- autoReport$email[!autoReport$email == input$email]
    })

    shiny::observeEvent(org(), {
      print(paste("Org value is now changed:", org()))
    })

    shiny::observeEvent(input$makeAutoReport, {

      report <- reports[[input$report]]
      interval <- strsplit(input$freq, "-")[[1]][2]

      if (type %in% c("subscription") | is.null(orgs)) {
        paramValues <- report$paramValues
        email <- rapbase::getUserEmail(session)
      } else {
        paramValues <- paramValues()
        email <- autoReport$email
      }

      rapbase::createAutoReport(
        synopsis = report$synopsis,
        package = registryName,
        type = type,
        fun = report$fun,
        paramNames = report$paramNames,
        paramValues = paramValues,
        owner = rapbase::getUserName(session),
        ownerName = rapbase::getUserFullName(session),
        email = email,
        organization = rapbase::getUserReshId(session),
        runDayOfYear = rapbase::makeRunDayOfYearSequence(
          interval = interval,
          startDay = input$start
        ),
        interval = interval,
        intervalName = strsplit(input$freq, "-")[[1]][1]
      )
      autoReport$tab <-
        rapbase::makeAutoReportTab(session, namespace = id, type = type,
                                   mapOrgId = orgList2df(orgs))
      autoReport$email <- vector()
    })

    shiny::observeEvent(input$edit_button, {
      repId <- strsplit(input$edit_button, "_")[[1]][2]
      rep <- rapbase::readAutoReportData()[[repId]]

      # try matching report by synopsis, fallback to currently selected
      for (i in names(reports)) {
        if (reports[[i]]$synopsis == rep$synopsis) {
          autoReport$report <- i
        }
      }
      autoReport$org <- rep$organization
      autoReport$freq <- paste0(rep$intervalName, "-", rep$interval)
      autoReport$email <- rep$email
      rapbase::deleteAutoReport(repId)
      autoReport$tab <-
        rapbase::makeAutoReportTab(session, namespace = id, type = type,
                                   mapOrgId = orgList2df(orgs))


      if (rep$type == "subscription") {

      }
      if (rep$type == "dispatchment") {

      }
      if (rep$type == "bulletin") {

      }
    })

    shiny::observeEvent(input$del_button, {
      repId <- strsplit(input$del_button, "_")[[1]][2]
      rapbase::deleteAutoReport(repId)
      autoReport$tab <-
        rapbase::makeAutoReportTab(session, namespace = id, type = type,
                                   mapOrgId = orgList2df(orgs))
    })

    # outputs
    output$reports <- shiny::renderUI({
      if (is.null(reports)) {
        NULL
      } else {
        shiny::selectInput(
          shiny::NS(id, "report"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("file")),
                        "Velg rapport:")
          ),
          choices = names(reports),
          selected = autoReport$report)
      }
    })

    output$synopsis <- shiny::renderUI({
      shiny::req(input$report)
      shiny::HTML(
        paste0(
          "Rapportbeskrivelse:<br/><i>",
          reports[[input$report]]$synopsis,
          "</i>"
        )
      )
    })

    output$freq <- shiny::renderUI({
      shiny::selectInput(
        shiny::NS(id, "freq"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("clock")), "Frekvens:")
        ),
        choices = list("\u00C5rlig" = "\u00C5rlig-year",
                        "Kvartalsvis" = "Kvartalsvis-quarter",
                        "M\u00E5nedlig" = "M\u00E5nedlig-month",
                        "Ukentlig" = "Ukentlig-week",
                        "Daglig" = "Daglig-day"),
        selected = autoReport$freq
      )
    })

    output$start <- shiny::renderUI({
      shiny::req(input$freq)
      shiny::dateInput(
        shiny::NS(id, "start"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("calendar")),
                      "F\u00F8rste utsending:")
        ),
        value = seq.Date(Sys.Date(),
                         by = strsplit(input$freq, "-")[[1]][2],
                         length.out = 2)[2]
      )
    })

    output$email <- shiny::renderUI({
      if (type %in% c("dispatchment", "bulletin")) {
        shiny::textInput(
          shiny::NS(id, "email"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("at")), "E-post mottaker:")
          ),
          value = "gyldig@epostadresse.no")
      } else {
        NULL
      }
    })

    output$editEmail <- shiny::renderUI({
      if (type %in% c("dispatchment", "bulletin")) {
        shiny::req(input$email)
        if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
                   input$email)) {
          NULL
        } else {
          if (input$email %in% autoReport$email) {
            shiny::actionButton(
              shiny::NS(id, "delEmail"),
              shiny::HTML(paste("Slett mottaker<br/>", input$email)),
              icon = shiny::icon("minus-square"))
          } else {
            shiny::actionButton(
              shiny::NS(id, "addEmail"),
              shiny::HTML(paste("Legg til mottaker<br/>", input$email)),
              icon = shiny::icon("plus-square"))
          }
        }
      } else {
        NULL
      }
    })

    output$recipient <- shiny::renderUI({
      if (type %in% c("dispatchment", "bulletin")) {
        recipientList <- paste0(autoReport$email, sep = "<br/>", collapse = "")
        shiny::HTML(paste0("Mottakere:<br/><i/>", recipientList))
      } else {
        NULL
      }
    })

    output$makeAutoReport <- shiny::renderUI({
      if (is.null(autoReport$report)) {
        NULL
      } else {
        if (type %in% c("subscription")) {
          shiny::actionButton(shiny::NS(id, "makeAutoReport"),
                              "Lag oppf\u00F8ring",
                              icon = shiny::icon("save"))
        } else {
          shiny::req(input$email)
          if (length(autoReport$email) == 0) {
            NULL
          } else {
            shiny::actionButton(shiny::NS(id, "makeAutoReport"),
                                "Lag oppf\u00F8ring",
                                icon = shiny::icon("save"))
          }
        }
      }
    })

    output$activeReports <- DT::renderDataTable(
      autoReport$tab, server = FALSE, escape = FALSE, selection = "none",
      rownames = FALSE,
      options = list(
        dom = "tp", ordering = FALSE,
        language = list(
          lengthMenu = "Vis _MENU_ rader per side",
          search = "S\u00f8k:",
          info = "Rad _START_ til _END_ av totalt _TOTAL_",
          paginate = list(previous = "Forrige", `next` = "Neste")
        )
      )
    )

    output$autoReportTable <- shiny::renderUI({
      if (length(autoReport$tab) == 0) {
        shiny::tagList(
          shiny::h2("Det finnes ingen oppf\u00F8ringer"),
          shiny::p(paste("Nye oppf\u00F8ringer kan lages fra menyen til",
                         "venstre, se veiledingen under.")),
          shiny::htmlOutput(shiny::NS(id, "autoReportGuide"))
        )
      } else {
        shiny::tagList(
          shiny::h2("Aktive oppf\u00F8ringer:"),
          DT::dataTableOutput(shiny::NS(id, "activeReports")),
          shiny::htmlOutput(shiny::NS(id, "autoReportGuide"))
        )
      }
    })

    output$autoReportGuide <- shiny::renderUI({
      rapbase::renderRmd(
        sourceFile = system.file("autoReportGuide.Rmd", package = "rapbase"),
        outputType = "html_fragment",
        params = list(registryName = registryName, type = type))
    })

    # return reactive values
    list(
      report = shiny::reactive(input$report),
      org = shiny::reactive(input$org),
      format = shiny::reactive(input$format)
    )
  })
}

#' @rdname autoReport
#' @export
autoReportApp <- function(registryName = "rapbase", type = "subscription",
                          reports = NULL, orgs = NULL) {
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        autoReportOrgInput("test"),
        autoReportFormatInput("test"),
        autoReportInput("test")
      ),
      shiny::mainPanel(autoReportUI("test"))
    )
  )

  server <- function(input, output, session) {

    org <- autoReportOrgServer("test", orgs)
    format <- autoReportFormatServer("test")

    paramValues <- shiny::reactive(c(org$value(), format()))

    ar <- autoReportServer(
      id = "test", registryName = registryName, type = type, org = org$value,
      paramValues = paramValues, reports = reports, orgs = orgs
    )

    shiny::observeEvent(ar$format(), {
      print(paste("selected file format is:", ar$format()))
    })
  }

  shiny::shinyApp(ui, server)
}


#' @rdname autoReport
#' @export
orgList2df <- function(orgs) {

  data.frame(
    name = names(orgs),
    id = as.vector(sapply(orgs, unname))
  )
}
