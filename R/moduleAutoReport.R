#' Auto Report Main UI
#'
#' User interface module for displaying existing auto reports and related
#' information.
#'
#' @param id Character string providing the Shiny module id.
#'
#' @return A Shiny UI object.
#'
#' @export
autoReportUI <- function(id) {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "autoReportTable"))
  )
}

#' Organization Selection UI
#'
#' User interface module for selecting an organization used as data source
#' for dispatchments and bulletins.
#'
#' @param id Character string providing the Shiny module id.
#'
#' @return A Shiny UI object.
#'
#' @export
autoReportOrgInput <- function(id) {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "orgs"))
  )
}

#' Organization Selection Server
#'
#' Server module for organization selection.
#'
#' @param id Character string providing the Shiny module id.
#' @param orgs Named list containing organization names and ids.
#'
#' @return A list containing:
#' \describe{
#' \item{name}{Reactive organization name.}
#' \item{value}{Reactive organization id.}
#' }
#'
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

#' Report Format Selection UI
#'
#' User interface module for selecting report output format.
#'
#' @param id Character string providing the Shiny module id.
#'
#' @return A Shiny UI object.
#'
#' @export
autoReportFormatInput <- function(id) {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "format"))
  )
}

#' Report Format Selection Server
#'
#' Server module for selecting report output format.
#'
#' @param id Character string providing the Shiny module id.
#'
#' @return Reactive value containing the selected report format.
#'
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

#' Auto Report Configuration UI
#'
#' User interface module for configuring and creating auto reports.
#'
#' @param id Character string providing the Shiny module id.
#'
#' @return A Shiny UI object.
#'
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
    shiny::uiOutput(shiny::NS(id, "makeAutoReport")),
    shiny::uiOutput(shiny::NS(id, "runAutoreport"))
  )
}

#' Auto Report Server Module
#'
#' Creates and manages automated report subscriptions, dispatchments,
#' and bulletins.
#'
#' @details
#' The `reports` argument must be a list where each entry represents
#' one report configuration:
#'
#' \describe{
#'   \item{synopsis}{Description of the report.}
#'   \item{fun}{Exported report function name.}
#'   \item{paramNames}{Function argument names.}
#'   \item{paramValues}{Corresponding argument values.}
#' }
#'
#' @param id Character string providing the shiny module id.
#' @param registryName Character string with registry package name.
#' @param type One of `"subscription"`, `"dispatchment"` or `"bulletin"`.
#' @param org Reactive organization id.
#' @param paramNames Reactive vector of parameter names.
#' @param paramValues Reactive vector of parameter values.
#' @param reports Report metadata list.
#' @param orgs Named list of organizations.
#' @param eligible Reactive logical indicating module availability.
#' @param freq Default report frequency.
#' @param user User metadata reactives.
#' @param runAutoReportButton Logical indicating if testing button should be
#' shown.
#'
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
#'     reports = reports, orgs = orgs, eligible = shiny::reactiveVal(TRUE), freq = "month", user
#'   )
#' }
#'
#' # run the shiny app in an interactive environment
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
#'
#' @return A Shiny server module.
#'
#' @export
autoReportServer <- function(
  id,
  registryName,
  type,
  org = NULL,
  paramNames = shiny::reactiveVal(c("")),
  paramValues = shiny::reactiveVal(c("")),
  reports = NULL,
  orgs = NULL,
  eligible = shiny::reactiveVal(TRUE),
  freq = "month",
  user,
  runAutoReportButton = FALSE
) {
  stopifnot(
    all(unlist(lapply(user, shiny::is.reactive), use.names = FALSE))
  )
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
  if (!shiny::is.reactive(reports)) {
    # make reports reactive if not already
    reports <- shiny::reactiveVal(reports)
  }

  shiny::moduleServer(id, function(input, output, session) {
    autoReport <- shiny::reactiveValues(
      tab = makeAutoReportTab(
        namespace = id,
        user = NULL,
        group = registryName,
        orgId = NULL,
        type = type,
        mapOrgId = orgList2df(orgs)
      ),
      org = unlist(orgs, use.names = FALSE)[1],
      freq = defaultFreq,
      email = vector()
    )

    ## update tab whenever changes to user privileges (and on init)
    userEvent <- shiny::reactive(
      list(user$name(), user$org(), user$role())
    )
    shiny::observeEvent(
      userEvent(),
      autoReport$tab <- makeAutoReportTab(
        namespace = id,
        user = user$name(),
        group = registryName,
        orgId = user$org(),
        type = type,
        mapOrgId = orgList2df(orgs)
      ),
      ignoreNULL = FALSE
    )

    shiny::observeEvent(input$addEmail, {
      autoReport$email <- c(autoReport$email, input$email)
    })

    shiny::observeEvent(input$delEmail, {
      autoReport$email <- autoReport$email[!autoReport$email == input$email]
    })

    shiny::observeEvent(input$makeAutoReport, {
      report <- reports()[[input$report]]
      interval <- strsplit(input$freq, "-")[[1]][2]
      paramValues <- report$paramValues
      paramNames <- report$paramNames

      if (type %in% c("subscription") || is.null(orgs)) {
        email <- user$email()
        organization <- user$org()
      } else {
        organization <- org()
        email <- autoReport$email
      }
      if (!paramValues()[1] == "") {
        stopifnot(length(paramNames()) == length(paramValues()))
        for (i in seq_along(paramNames())) {
          paramValues[paramNames == paramNames()[i]] <- paramValues()[i]
        }
      }

      createAutoReport(
        synopsis = report$synopsis,
        package = registryName,
        type = type,
        fun = report$fun,
        paramNames = report$paramNames,
        paramValues = paramValues,
        owner = user$name(),
        ownerName = user$fullName(),
        email = email,
        organization = organization,
        runDayOfYear = as.integer(c(10, 42, 100)), # Not in use anymore
        startDate = input$start,
        interval = interval,
        intervalName = strsplit(input$freq, "-")[[1]][1]
      )
      autoReport$tab <-
        makeAutoReportTab(
          namespace = id,
          user = user$name(),
          group = registryName,
          orgId = user$org(),
          type = type,
          mapOrgId = orgList2df(orgs)
        )
      autoReport$email <- vector()
    })

    rv <- shiny::reactiveValues(
      pending_repId_edit = NULL
    )
    shiny::observeEvent(input$edit_button, {

      btn_id <- input$edit_button$id
      repId  <- strsplit(btn_id, "__", fixed = TRUE)[[1]][2]

      rv$pending_repId_edit <- repId

      shiny::showModal(
        shiny::modalDialog(
          title = "Bekreft endring",
          "Eposten legges tilbake i epostfeltet og oppf\u00F8ringen slettes.
          Du kan gj\u00F8re endringer og deretter legge til eposten
          igjen ved \u00E5 trykke \"Lag oppf\u00F8ring\".
          Er du sikker p\u00E5 at du vil redigere mottaker(e)?",
          footer = shiny::tagList(
            shiny::modalButton("Avbryt"),
            shiny::actionButton(shiny::NS(id, "confirm_edit"),
                                "Ja, rediger", class = "btn-primary")
          ),
          easyClose = TRUE
        )
      )
    })
    shiny::observeEvent(input$confirm_edit, {
      shiny::removeModal()

      repId <- rv$pending_repId_edit
      rv$pending_repId_edit <- NULL
      if (is.null(repId) || !nzchar(repId)) return()

      repIdSplit <- strsplit(repId, "\n", fixed = TRUE)[[1]]

      rep <- readAutoReportData() |>
        dplyr::filter(id %in% repIdSplit)

      if (nrow(rep) == 0) {
        message("Can not modify (0 rows)")
        return(NULL)
      }

      autoReport$org   <- rep$organization
      autoReport$freq  <- paste0(rep$intervalName, "-", rep$interval)
      autoReport$email <- rep$email

      deleteAutoReport(repId)

      autoReport$tab <- makeAutoReportTab(
        namespace = id,
        user = user$name(),
        group = registryName,
        orgId = user$org(),
        type = type,
        mapOrgId = orgList2df(orgs)
      )
    })

    rv <- shiny::reactiveValues(pending_repId_del = NULL)
    shiny::observeEvent(input$del_button, {
      btn_id <- input$del_button$id

      repId <- strsplit(btn_id, "__", fixed = TRUE)[[1]][2]

      rv$pending_repId_del <- repId

      shiny::showModal(
        shiny::modalDialog(
          title = "Bekreft sletting",
          "Er du sikker p\u00E5 at du vil slette mottaker(e)?",
          footer = shiny::tagList(
            shiny::modalButton("Avbryt"),
            shiny::actionButton(shiny::NS(id, "confirm_delete"),
                                "Ja, slett", class = "btn-danger")
          ),
          easyClose = TRUE
        )
      )
    })

    shiny::observeEvent(input$confirm_delete, {
      shiny::removeModal()

      repId <- rv$pending_repId_del
      rv$pending_repId_del <- NULL
      if (is.null(repId) || !nzchar(repId)) return()

      deleteAutoReport(repId)

      autoReport$tab <- makeAutoReportTab(
        namespace = id,
        user = user$name(),
        group = registryName,
        orgId = user$org(),
        type = type,
        mapOrgId = orgList2df(orgs)
      )
    })

    # outputs
    output$reports <- shiny::renderUI({
      if (is.null(reports())) {
        NULL
      } else {
        shiny::selectInput(
          shiny::NS(id, "report"),
          label = shiny::tags$div(
            shiny::HTML(as.character(shiny::icon("file")), "Velg rapport:")
          ),
          choices = names(reports()),
          selected = names(reports())[1]
        )
      }
    })

    output$synopsis <- shiny::renderUI({
      shiny::req(input$report)
      shiny::HTML(
        paste0(
          "Rapportbeskrivelse:<br/><i>",
          reports()[[input$report]]$synopsis,
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
        # set default to following day
        value = Sys.Date() + 1,
        min = Sys.Date() + 1,
        max = seq.Date(Sys.Date(), length.out = 2, by = "1 years")[2] - 1,
        weekstart = 1,
        language = "no"
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
      if (is.null(reports()) || !eligible()) {
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
        pageLength = 40,
        language = list(
          lengthMenu = "Vis _MENU_ rader per side",
          search = "S\u00f8k:",
          info = "Rad _START_ til _END_ av totalt _TOTAL_",
          paginate = list(previous = "Forrige", `next` = "Neste")
        )
      )
    )

    output$autoReportTable <- shiny::renderUI({
      if (!eligible()) {
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

    # Option to run all auto reports with a given date by clicking a button.
    # This is only available if runAutoReportButton = TRUE
    output$runAutoreport <- shiny::renderUI({
      if (runAutoReportButton) {
        shiny::tagList(
          shiny::hr(),
          shiny::h3("Kj\u00F8r alle aktuelle autorapporter"),
          shiny::p(paste0(
            "Denne funksjonen er kun for testing og utvikling, ",
            "og vil lage alle rapporter for gitt dato."
          )),
          shiny::actionButton(
            inputId = shiny::NS(id, "run_autoreport"),
            label = "Kj\u00F8r autorapporter",
            icon = shiny::icon("play"),
            onclick = "this.disabled=true;"
          ),
          shiny::dateInput(
            inputId = shiny::NS(id, "rapportdato"),
            label = "Kj\u00F8r rapporter med dato:",
            value = Sys.Date() + 1,
            weekstart = 1,
            language = "no"
          ),
          shiny::checkboxInput(
            inputId = shiny::NS(id, "sendEmails"),
            label = "Send e-post",
            value = FALSE
          )
        )
      } else {
        NULL
      }
    })
    shiny::observeEvent(input$run_autoreport, {
      # Run all auto reports with the given date
      # when clicking the button
      dato <- input$rapportdato
      message("Running all auto reports for date ", dato,
        " and registry ", registryName, ", ",
        ifelse(input$sendEmails, "WITH", "WITHOUT"),
        " sending e-mails. This job was triggered by ", user$fullName()
      )
      dryRun <- !(input$sendEmails)
      runAutoReport(
        group = registryName,
        dato = dato,
        dryRun = dryRun
      )
      message("Finished running all auto reports for date ", dato)
      # reactivate button
      shiny::updateActionButton(
        inputId = "run_autoreport",
        disabled = FALSE
      )
    })
  })
}

#' Alias for autoReportServer
#'
#' Backward compatible wrapper around [autoReportServer()].
#'
#' @param ... Arguments passed directly to [autoReportServer()].
#'
#' @return Same result as [autoReportServer()].
#'
#' @export
autoReportServer2 <- function(...) {
  autoReportServer(...)
}

#' Convert Organization List to Data Frame
#'
#' Internal utility for converting a named organization list into
#' a data frame.
#'
#' @param orgs Named list of organizations.
#'
#' @return Data frame with columns `name` and `id`.
#'
#' @keywords internal
orgList2df <- function(orgs) {
  data.frame(
    name = names(orgs),
    id = as.vector(sapply(orgs, unname))
  )
}
