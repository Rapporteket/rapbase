#' Shiny app with database export functionality
#'
#' @param registryName Character string registry name key, corresponding to
#' github team name
#'
#' @export
exportApp <- function(registryName = "") {
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
      rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE)
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
    user <- rapbase::navbarWidgetServer2(
      id = "navbar-widget",
      orgName = registryName,
      caller = registryName
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
        rapbase::exportUCInput("export")
      } else {
        return(NULL)
      }
    })

    rapbase::exportUCServer("export", registryName)

    # User guide
    output$exportMainPanel <- shiny::renderUI({
      if (user$role() == "SC") {
        rapbase::exportGuideUI("exportGuide")
      } else {
        return(NULL)
      }
    })

    rapbase::exportGuideServer("exportGuide", registryName)

  }
  shiny::shinyApp(ui, server)
}
