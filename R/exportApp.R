#' Shiny app with database export functionality
#'
#' @param repoName Character string registry name key, corresponding to
#' github team name
#' @param registryName Character string registry name key, can be used to
#' specify name of database if needed.
#' @export
exportApp <- function(repoName = "", registryName = "data") {
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
      rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
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
          shiny::uiOutput("exportDbUI"),
          shiny::uiOutput("exportSidebarPanel")
        ),
        shiny::mainPanel(
          shiny::uiOutput("exportMainPanel")

        )
      )
    )
  )
  server <- function(input, output, session) {
    user <- rapbase::navbarWidgetServer2(
      id = "navbar-widget",
      orgName = repoName,
      caller = repoName
    )

    shiny::observeEvent(user$role(), {
      shiny::hideTab(inputId = "navbarpage", target = "exportPanel")
      if (user$role() == "SC") {
        shiny::showTab(inputId = "navbarpage", target = "exportPanel")
      }
    })

    # User control
    output$exportSidebarPanel <- shiny::renderUI({
      if (user$role() == "SC") {
        rapbase::exportUCInput("export")
      } else {
        return(NULL)
      }
    })

    rapbase::exportUCServer("export", registryName = input$exportDb,
                            repoName = repoName)

    # User guide
    output$exportMainPanel <- shiny::renderUI({
      if (user$role() == "SC") {
        rapbase::exportGuideUI("exportGuide")
      } else {
        return(NULL)
      }
    })

    output$exportDbUI <- shiny::renderUI({
      shiny::selectInput(
        "exportDb",
        label = shiny::tags$div(
          shiny::HTML(as.character(shiny::icon("database")), "Velg database:")
        ),
        choices = strsplit(Sys.getenv("MYSQL_DB_DATA"), ",")[[1]]
      )
    })

    rapbase::exportGuideServer("exportGuide", repoName)

    ## Metadata
    meta <- shiny::reactive({
      rapbase::describeRegistryDb(registryName = registryName)
    })

    meta2 <- shiny::reactive({
      rapbase::nlinesRegistryDb(registryName = registryName)
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
        meta2()[[shiny::req(input$metaTab)]]$n_lines,
        " linjer"
      )
      )
    })

  }
  shiny::shinyApp(ui, server)
}
