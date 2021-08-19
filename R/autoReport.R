#' Shiny modules and helper functions for registry auto reports
#'
#' These shiny modules may be used to set up auto reporting from registries at
#' Rapporteket
#'
#' @param id Character string shiny module id
#' @param registryName Character string registry name key. Must correspond to
#' the R package name
#' @param type Character string defining the type of auto reports. Must be one
#' of \code{c("subscription", "dispatchment", "bulletin")}
#' @param reports List of reports that will be provided as automated reports.
#' Describe further...
#' @param orgs List of named organizations and values. Describe further...
#'
#' @return Shiny objects, mostly. Helper functions may return other stuff too.
#' @name autoReport
#' @aliases autoReportUI autoReportInput autoReportServer autoReportApp
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
autoReportInput <- function(id) {

  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "reports")),
    shiny::uiOutput(shiny::NS(id, "orgs")),
    shiny::uiOutput(shiny::NS(id, "freq")),
    shiny::uiOutput(shiny::NS(id, "start")),
    shiny::uiOutput(shiny::NS(id, "format")),
    shiny::uiOutput(shiny::NS(id, "email")),
    shiny::htmlOutput(shiny::NS(id, "editEmail")),
    shiny::htmlOutput(shiny::NS(id, "recipient")),
    shiny::tags$hr(),
    shiny::uiOutput(shiny::NS(id, "makeAutoReport"))
  )
}

#' @rdname autoReport
#' @export
autoReportServer <- function(id, registryName, type, reports = NULL,
                             orgs = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    autoReport <- shiny::reactiveValues(
      tab = rapbase::makeAutoReportTab(session = session, namespace = id,
                                       type = type, mapOrgId = NULL),
      report = names(reports)[1],
      org = unlist(orgs, use.names = FALSE)[1],
      freq = "Månedlig-month",
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
      if (type %in% c("subscription") | is.null(orgs)) {
        organization <- rapbase::getUserReshId(session)
        email <- rapbase::getUserEmail(session)
      } else {
        organization <- input$org
        email <- autoReport$email
      }

      rapbase::createAutoReport(
        synopsis = report$synopsis,
        package = registryName,
        type = type,
        fun = report$fun,
        paramNames = report$paramNames,
        paramValues = report$paramValues,
        owner = rapbase::getUserName(session),
        ownerName = rapbase::getUserFullName(session),
        email = email,
        organization = organization,
        runDayOfYear = rapbase::makeRunDayOfYearSequence(
          interval = interval,
          startDay = input$start
        ),
        interval = interval,
        intervalName = strsplit(input$freq, "-")[[1]][1]
      )
      autoReport$tab <-
        rapbase::makeAutoReportTab(session, namespace = id, type = type,
                                   mapOrgId = NULL)
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
                                   mapOrgId = NULL)


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
        rapbase::makeAutoReportTab(session, namespace= id, type = type,
                                   mapOrgId = NULL)
    })

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

    output$orgs <- shiny::renderUI({
      if (type %in% c("subscription") | is.null(orgs)) {
        NULL
      } else {
        shiny::selectInput(
          shiny::NS(id, "org"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("database")),
                        "Velg datakilde:")
          ),
          choices = orgs,
          selected = autoReport$org
        )
      }
    })

    output$freq <- shiny::renderUI({
      shiny::selectInput(
        shiny::NS(id, "freq"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("clock")), "Frekvens:")
        ),
        choices = list(Årlig = "Årlig-year",
                        Kvartalsvis = "Kvartalsvis-quarter",
                        Månedlig = "Månedlig-month",
                        Ukentlig = "Ukentlig-week",
                        Daglig = "Daglig-day"),
        selected = autoReport$freq
      )
    })

    output$start <- shiny::renderUI({
      shiny::req(input$freq)
      shiny::dateInput(
        shiny::NS(id, "start"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("calendar")),
                      "Første utsending:")
        ),
        value = seq.Date(Sys.Date(),
                         by = strsplit(input$freq, "-")[[1]][2],
                         length.out = 2)[2]
      )
    })

    output$format <- shiny::renderUI({
      if (type %in% c("subscription")) {
      shiny::selectInput(
        shiny::NS(id, "format"),
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("file-pdf")), "Fil-format:")
        ),
        choices = c("html", "pdf"))
      } else {
        NULL
      }
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
        shiny::HTML(paste0("<b>Mottakere:</b><br/>", recipientList))
      } else {
        NULL
      }
    })

    output$makeAutoReport <- shiny::renderUI({
      if (type %in% c("subscription")) {
        shiny::actionButton(shiny::NS(id, "makeAutoReport"), "Lag oppføring",
                            icon = shiny::icon("save"))
      } else {
        shiny::req(input$email)
        if (length(autoReport$email) == 0) {
          NULL
        } else {
          shiny::actionButton(shiny::NS(id, "makeAutoReport"), "Lag oppføring",
                              icon = shiny::icon("save"))
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
          shiny::h2("Det finnes ingen oppføringer"),
          shiny::p(paste("Nye oppføringer kan lages fra menyen til venstre,",
                         "se veiledingen under.")),
          shiny::htmlOutput(shiny::NS(id, "autoReportGuide"))
        )
      } else {
        shiny::tagList(
          shiny::h2("Aktive oppføringer:"),
          DT::dataTableOutput(shiny::NS(id, "activeReports")),
          shiny::htmlOutput(shiny::NS(id, "autoReportGuide"))
        )
      }
    })

    output$autoReportGuide <- shiny::renderUI({
      rapbase::renderRmd(
        sourceFile = system.file("autoReportGuide.Rmd", package = "rapbase"),
        outputType = "html_fragment",
        params = list(registryName = registryName))
    })
  })

}

#' @rdname autoReport
#' @export
autoReportApp <- function(registryName = "rapbase", type = "subscription",
                          reports = NULL, orgs = NULL) {
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(autoReportInput("test")),
      shiny::mainPanel(autoReportUI("test"))
    )
  )

  server <- function(input, output, session) {
    autoReportServer(id = "test", registryName = registryName, type = type,
                     reports = reports, orgs = orgs)
  }

  shiny::shinyApp(ui, server)
}
