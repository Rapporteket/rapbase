#' Shiny modules and helper functions for registry auto reports
#'
#' These shiny modules may be used to set up auto reporting from registries at
#' Rapporteket.
#'
#' The \emph{reports} argument must be a list where each entry
#' represents one report and its name will be used in the auto report user
#' interface for selecting reports, \emph{e.g.}
#' \code{reports = list(MagicReport = ...)} will produce the entry "MagicReport"
#' in the GUI list of reports to select from. The value of each entry must be
#' another list with the following names and values:
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
#' go unchanged. For dispatchments and bulletins it is likely that parameter
#' values must be set dynamically in which case \emph{paramValues} must be
#' a reactive part of the application. See Examples on how function arguments
#' may be used as reactives in an application.
#'
#' @param id Character string providing the shiny module id.
#' @param registryName Character string with the registry name key. Must
#' correspond to the registry R package name.
#' @param type Character string defining the type of auto reports. Must be one
#' of \code{c("subscription", "dispatchment", "bulletin")}
#' @param reports List of a given structure that provides meta data for the
#' reports that are made available as automated reports. See Details for further
#' description.
#' @param org Shiny reactive or NULL (default) defining the organization (id)
#' of the data source used for dispatchments and bulletins (in which case it
#' cannot be set to NULL) and its value will be used to populate the
#' \emph{organization} field in auto report data (autoReport.yml) for these auto
#' report types. On the other hand, since subscriptions are personal (per user)
#' the only relevant organization id will implicit be that of the user and in
#' this case any value of \code{org} will be disregarded.
#' @param paramNames Shiny reactive value as a vector of parameter names of
#' which values are to be set interactively at application run time. Each
#' element of this vector must match exactly those of \code{paramValues}.
#' Default value is \code{shiny::reactiveVal("")}.
#' @param paramValues Shiny reactive value as a vector of those parameter values
#' to be set interactively, \emph{i.e.} as per user input in the application.
#' Default value is set to \code{shiny::reactiveVal("")} in which case parameter
#' values defined in \code{reports} will be used as is. In other words,
#' explicit use of \code{paramValues} will only be needed if parameter values
#' must be changed during application run time. If so, each element of this
#' vector must correspond exactly to those of \code{paramNames}.
#' @param orgs Named list of organizations (names) and ids (values). When set to
#' \code{NULL} (default) the ids found in auto report data will be used in the
#' table listing existing auto reports.
#' @param eligible Logical defining if the module should be allowed to work at
#' full capacity. This might be useful when access to module products should be
#' restricted. Default is TRUE, \emph{i.e.} no restrictions.
#' @param freq Character string defining default frequency set in the auto
#' report GUI. Must be one of
#' \code{c("day", "week", "month", "quarter", "year")}. Default value is
#' "month".
#'
#'
#' @return In general, shiny objects. In particular, \code{autoreportOrgServer}
#' returns a list with names "name" and "value" with corresponding reactive
#' values for the selected organization name and id. This may be used when
#' parameter values of auto report functions needs to be altered at application
#' run time. \code{orgList2df} returns a data frame with columns "name" and
#' "id".
#' @name autoReport
#' @aliases autoReportUI autoReportOrgInput autoReportOrgServer
#' autoReportFormatInput autoReportFormatSercer autoReportInput autoReportServer
#' autoReportApp orgList2df
#' @examples
#' ## make a list for report metadata
#' reports <- list(
#'   FirstReport = list(
#'     synopsis = "First example report",
#'     fun = "fun1",
#'     paramNames = c("organization", "topic", "outputFormat"),
#'     paramValues = c(111111, "work", "html")
#'   ),
#'   SecondReport = list(
#'     synopsis = "Second example report",
#'     fun = "fun2",
#'     paramNames = c("organization", "topic", "outputFormat"),
#'     paramValues = c(111111, "leisure", "pdf")
#'   )
#' )
#'
#' ## make a list of organization names and numbers
#' orgs <- list(
#'   OrgOne = 111111,
#'   OrgTwo = 222222
#' )
#'
#' ## client user interface function
#' ui <- shiny::fluidPage(
#'   shiny::sidebarLayout(
#'     shiny::sidebarPanel(
#'       autoReportFormatInput("test"),
#'       autoReportOrgInput("test"),
#'       autoReportInput("test")
#'     ),
#'     shiny::mainPanel(
#'       autoReportUI("test")
#'     )
#'   )
#' )
#'
#' ## server function
#' server <- function(input, output, session) {
#'   org <- autoReportOrgServer("test", orgs)
#'   format <- autoReportFormatServer("test")
#'
#'   # set reactive parameters overriding those in the reports list
#'   paramNames <- shiny::reactive(c("organization", "outputFormat"))
#'   paramValues <- shiny::reactive(c(org$value(), format()))
#'
#'   autoReportServer(
#'     id = "test", registryName = "rapbase", type = "dispatchment",
#'     org = org$value, paramNames = paramNames, paramValues = paramValues,
#'     reports = reports, orgs = orgs, eligible = TRUE
#'   )
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
          shiny::HTML(
            as.character(shiny::icon("database")),
            "Velg datakilde:"
          )
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
autoReportServer <- function(id, registryName, type, org = NULL,
                             paramNames = shiny::reactiveVal(c("")),
                             paramValues = shiny::reactiveVal(c("")),
                             reports = NULL, orgs = NULL, eligible = TRUE,
                             freq = "month") {
  if (!type %in% c("subscription")) {
    stopifnot(shiny::is.reactive(org))
    stopifnot(shiny::is.reactive(paramNames))
    stopifnot(shiny::is.reactive(paramValues))
  }

  stopifnot(freq %in% c("day", "week", "month", "quarter", "year"))

  defaultFreq <- switch(freq,
    day = "Daglig-day",
    week = "Ukentlig-week",
    month = "M\u00E5nedlig-month",
    quarter = "Kvartalsvis-quarter",
    year = "\u00C5rlig-year"
  )

  shiny::moduleServer(id, function(input, output, session) {
    autoReport <- shiny::reactiveValues(
      tab = makeAutoReportTab(
        session = session,
        namespace = id,
        type = type,
        mapOrgId = orgList2df(orgs)
      ),
      report = names(reports)[1],
      org = unlist(orgs, use.names = FALSE)[1],
      freq = defaultFreq,
      email = vector()
    )

    shiny::observeEvent(input$addEmail, {
      autoReport$email <- c(autoReport$email, input$email)
    })

    shiny::observeEvent(input$delEmail, {
      autoReport$email <- autoReport$email[!autoReport$email == input$email]
    })

    shiny::observeEvent(input$makeAutoReport, {
      report <- reports[[input$report]]
      interval <- strsplit(input$freq, "-")[[1]][2]
      paramValues <- report$paramValues
      paramNames <- report$paramNames

      if (type %in% c("subscription") | is.null(orgs)) {
        email <- getUserEmail(session)
        organization <- getUserReshId(session)
      } else {
        organization <- org()
        if (!paramValues()[1] == "") {
          stopifnot(length(paramNames()) == length(paramValues()))
          for (i in seq_len(length(paramNames()))) {
            paramValues[paramNames == paramNames()[i]] <- paramValues()[i]
          }
        }
        email <- autoReport$email
      }

      createAutoReport(
        synopsis = report$synopsis,
        package = registryName,
        type = type,
        fun = report$fun,
        paramNames = report$paramNames,
        paramValues = paramValues,
        owner = rapbase::getUserName(session),
        ownerName = rapbase::getUserFullName(session),
        email = email,
        organization = organization,
        runDayOfYear = makeRunDayOfYearSequence(
          interval = interval,
          startDay = input$start
        ),
        startDate = input$start,
        interval = interval,
        intervalName = strsplit(input$freq, "-")[[1]][1]
      )
      autoReport$tab <-
        makeAutoReportTab(
          session,
          namespace = id,
          type = type,
          mapOrgId = orgList2df(orgs)
        )
      autoReport$email <- vector()
    })

    shiny::observeEvent(input$edit_button, {
      repId <- strsplit(input$edit_button, "__")[[1]][2]
      rep <- readAutoReportData()[[repId]]

      # try matching report by synopsis, fallback to currently selected
      for (i in names(reports)) {
        if (reports[[i]]$synopsis == rep$synopsis) {
          autoReport$report <- i
        }
      }
      autoReport$org <- rep$organization
      autoReport$freq <- paste0(rep$intervalName, "-", rep$interval)
      autoReport$email <- rep$email
      deleteAutoReport(repId)
      autoReport$tab <- makeAutoReportTab(
        session,
        namespace = id,
        type = type,
        mapOrgId = orgList2df(orgs)
      )

      if (rep$type == "subscription") {

      }
      if (rep$type == "dispatchment") {

      }
      if (rep$type == "bulletin") {

      }
    })

    shiny::observeEvent(input$del_button, {
      repId <- strsplit(input$del_button, "__")[[1]][2]
      deleteAutoReport(repId)
      autoReport$tab <- makeAutoReportTab(
        session,
        namespace = id,
        type = type,
        mapOrgId = orgList2df(orgs)
      )
    })

    # outputs
    output$reports <- shiny::renderUI({
      if (is.null(reports)) {
        NULL
      } else {
        shiny::selectInput(
          shiny::NS(id, "report"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("file")), "Velg rapport:")
          ),
          choices = names(reports),
          selected = autoReport$report
        )
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
        choices = list(
          "Aarlig" = "\u00C5rlig-year",
          "Kvartalsvis" = "Kvartalsvis-quarter",
          "Maanedlig" = "M\u00E5nedlig-month",
          "Ukentlig" = "Ukentlig-week",
          "Daglig" = "Daglig-day"
        ),
        selected = autoReport$freq
      )
    })

    output$start <- shiny::renderUI({
      shiny::req(input$freq)
      shiny::dateInput(
        shiny::NS(id, "start"),
        label = shiny::tags$div(
          shiny::HTML(
            as.character(shiny::icon("calendar")), "F\u00F8rste utsending:"
          )
        ),
        # if freq is year make first issue tomorrow, otherwise postpone by freq
        value = if (strsplit(input$freq, "-")[[1]][2] == "year") {
          Sys.Date() + 1
        } else {
          seq.Date(Sys.Date(),
            by = strsplit(input$freq, "-")[[1]][2],
            length.out = 2
          )[2]
        },
        min = Sys.Date() + 1,
        max = seq.Date(Sys.Date(), length.out = 2, by = "1 years")[2] - 1
      )
    })

    output$email <- shiny::renderUI({
      if (type %in% c("dispatchment", "bulletin")) {
        shiny::textInput(
          shiny::NS(id, "email"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("at")), "E-post mottaker:")
          ),
          value = "gyldig@epostadresse.no"
        )
      } else {
        NULL
      }
    })

    output$editEmail <- shiny::renderUI({
      if (type %in% c("dispatchment", "bulletin")) {
        shiny::req(input$email)
        if (!grepl(
          "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
          input$email
        )) {
          NULL
        } else {
          if (input$email %in% autoReport$email) {
            shiny::actionButton(
              shiny::NS(id, "delEmail"),
              shiny::HTML(paste("Slett mottaker<br/>", input$email)),
              icon = shiny::icon("minus-square")
            )
          } else {
            shiny::actionButton(
              shiny::NS(id, "addEmail"),
              shiny::HTML(paste("Legg til mottaker<br/>", input$email)),
              icon = shiny::icon("plus-square")
            )
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
      if (is.null(autoReport$report) || !eligible) {
        NULL
      } else {
        if (type %in% c("subscription")) {
          shiny::actionButton(
            shiny::NS(id, "makeAutoReport"),
            "Lag oppf\u00F8ring",
            icon = shiny::icon("save")
          )
        } else {
          shiny::req(input$email)
          if (length(autoReport$email) == 0) {
            NULL
          } else {
            shiny::actionButton(
              shiny::NS(id, "makeAutoReport"),
              "Lag oppf\u00F8ring",
              icon = shiny::icon("save")
            )
          }
        }
      }
    })

    output$activeReports <- DT::renderDataTable(
      autoReport$tab,
      server = FALSE, escape = FALSE, selection = "none",
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
      if (!eligible) {
        shiny::tagList(
          shiny::h2(paste0("Funksjonen ('", type, "') er utilgjengelig")),
          shiny::p("Ved sp\u00F8rsm\u00E5l ta gjerne kontakt med registeret."),
          shiny::hr()
        )
      } else if (length(autoReport$tab) == 0) {
        shiny::tagList(
          shiny::h2("Det finnes ingen oppf\u00F8ringer"),
          shiny::p(paste(
            "Nye oppf\u00F8ringer kan lages fra menyen til",
            "venstre. Bruk gjerne veiledingen under."
          )),
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
      renderRmd(
        sourceFile = system.file("autoReportGuide.Rmd", package = "rapbase"),
        outputType = "html_fragment",
        params = list(registryName = registryName, type = type)
      )
    })
  })
}

#' @rdname autoReport
#' @export
autoReportApp <- function(registryName = "rapbase", type = "subscription",
                          reports = NULL, paramNames = shiny::reactive(c("")),
                          orgs = NULL) {
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

    autoReportServer(
      id = "test", registryName = registryName, type = type,
      paramNames = paramNames, paramValues = paramValues, reports = reports,
      orgs = orgs
    )
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
