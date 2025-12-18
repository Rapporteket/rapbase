#' Shiny app with database export functionality
#'
#' @param teamName Character string, corresponding to
#' github team name
#' @param dbName Character string, can be used to
#' specify name of database if needed. Defaults to "data",
#' which will work for most registries.
#' @export
exportApp <- function(teamName = "", dbName = "data") {
  ui <- shiny::navbarPage(
    id = "navbarpage",
    title = shiny::div(shiny::a(shiny::includeHTML(
      system.file(
        "www/logo.svg",
        package = "rapbase"
      )
    )
    ),
    "Simple export app"),
    windowTitle = "Simple export app",
    theme = "rap/bootstrap.css",
    shiny::tabPanel(
      title = "Info",
      navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
      shiny::sidebarLayout(
        shiny::sidebarPanel(shiny::uiOutput("metaControl")),
        shiny::mainPanel(
          shiny::uiOutput("n_lines"),
          shiny::htmlOutput("metaData")
        )
      )
    ),
    shiny::tabPanel(
      title = "Eksport",
      value = "exportPanel",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::uiOutput("exportSidebarPanel")
        ),
        shiny::mainPanel(
          shiny::uiOutput("exportMainPanel")

        )
      )
    )
  )
  server <- function(input, output, session) {
    user <- navbarWidgetServer2(
      id = "navbar-widget",
      orgName = "exportApp"
    )

    shiny::observeEvent(user$role(), {
      if (user$role() == "SC") {
        shiny::showTab(inputId = "navbarpage", target = "exportPanel")
      } else {
        shiny::hideTab(inputId = "navbarpage", target = "exportPanel")
      }
    })

    # User control
    output$exportSidebarPanel <- shiny::renderUI({
      if (user$role() == "SC") {
        exportUCInput("export")
      } else {
        return(NULL)
      }
    })

    exportUCServer("export", dbName = dbName, teamName = teamName)

    # User guide
    output$exportMainPanel <- shiny::renderUI({
      if (user$role() == "SC") {
        exportGuideUI("exportGuide")
      } else {
        return(NULL)
      }
    })

    exportGuideServer("exportGuide", dbName)

    ## Metadata
    meta <- shiny::reactive({
      describeRegistryDb(registryName = dbName)
    })

    output$metaControl <- shiny::renderUI({
      tabs <- names(meta())
      shiny::selectInput("metaTab", "Velg tabell:", tabs)
    })

    output$metaDataTable <- DT::renderDataTable(
      meta()[[input$metaTab]], rownames = FALSE,
      options = list(lengthMenu = c(25, 50, 100, 200, 400))
    )

    output$metaData <- shiny::renderUI({
      DT::dataTableOutput("metaDataTable")
    })

    output$n_lines <- shiny::renderUI({
      shiny::h4(paste0(
        input$metaTab,
        " har ",
        nlinesRegistryDb(
          registryName = dbName,
          tab = shiny::req(input$metaTab)
        ),
        " linjer"
      )
      )
    })

  }
  shiny::shinyApp(ui, server)
}
