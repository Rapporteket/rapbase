
autoReportUI <- function(id) {

  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "autoReportTable"))
  )
}

autoReportInput <- function(id) {

  shiny::tagList(
    # shiny::uiOutput(shiny::NS(id, "autoReportList")),
    # shiny::uiOutput(shiny::NS(id, "issueFromOrgList")),
    shiny::uiOutput(shiny::NS(id, "freq")),
    shiny::uiOutput(shiny::NS(id, "start")),
    shiny::uiOutput(shiny::NS(id, "format")),
    shiny::uiOutput(shiny::NS(id, "email"), inline = TRUE),
    shiny::htmlOutput(shiny::NS(id, "editEmail")),
    shiny::htmlOutput(shiny::NS(id, "recipient")),
    shiny::tags$hr(),
    shiny::uiOutput(shiny::NS(id, "makeAutoReport"))
  )
}

autoReportServer <- function(id, registryName, type) {

  shiny::moduleServer(id, function(input, output, session) {

    autoReport <- shiny::reactiveValues(
      tab = rapbase::makeAutoReportTab(session = session, type = type,
                                       mapOrgId = NULL),
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
      autoReport$tab <-
        rapbase::makeAutoReportTab(session = session, type = type,
                                   mapOrgId = NULL)
    })


    output$freq <- shiny::renderUI({
      shiny::selectInput(
        shiny::NS(id, "freq"), "Frekvens:",
        list(Årlig = "Årlig-year",
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
      shiny::selectInput(shiny::NS(id, "format"), "Fil-format:",
                         c("html", "pdf"))
      } else {
        NULL
      }
    })

    output$email <- shiny::renderUI({
      if (type %in% c("dispatchment", "bulletin")) {
        shiny::textInput(shiny::NS(id, "email"), "E-post mottaker:",
                         placeholder = "skriv inn epost her")
      } else {
        NULL
      }
    })

    output$editEmail <- shiny::renderHTML({
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


autoReportApp <- function(registryName = "rapbase", type = "subscription") {
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(autoReportInput("test")),
      shiny::mainPanel(autoReportUI("test"))
    )
  )

  server <- function(input, output, session) {
    autoReportServer(id = "test", registryName = registryName, type = type)
  }

  shiny::shinyApp(ui, server)
}
