#' Shiny modules providing GUI and server logic for user info widget
#'
#' Shiny modules for making a user information widget in registry shiny apps at
#' Rapporteket. One benefit using these modules will be reduced complexity and
#' number of code lines for each registry.
#'
#' These modules take use of the shiny session object to obtain data for the
#' widget. Hence, a Rapporteket like context will be needed for these modules to
#' function properly.
#'
#' @param id Character string providing module namespace
#' @param addUserInfo Logical defing if an "about" hyperlink is to be added
#' @param orgName Character string naming the organization
#' @param caller Character string naming the environment this function was
#' called from. Default value is \code{environmentName(rlang::caller_env())}.
#' The value is used to display the current version of the R package
#' representing the registry at Rapporteket. If this module is called from
#' exported functions in the registry R package use the default value. If the
#' module is called from outside the registry environment \code{caller} must be
#' set to the actual name of the R package.
#'
#' @return Shiny objects, mostly. Helper functions may return other stuff too.
#' @name navbarWidget
#' @aliases navbarWidgetInput navbarWidgetServer navbarWidgetApp
#' @examples
#' ## client user interface function
#' ui <- shiny::tagList(
#'   shiny::navbarPage(
#'     "Testpage",
#'     shiny::tabPanel(
#'       "Testpanel",
#'       shiny::mainPanel(
#'         navbarWidgetInput("testWidget")
#'       )
#'     )
#'   )
#' )
#'
#' ## server function
#' server <- function(input, output, session) {
#'   navbarWidgetServer("testWidget", orgName = "Test org", caller = "Rpkg")
#' }
#'
#' ## run the app in an interactive session and a Rapporteket like environment
#' if (interactive() && isRapContext()) {
#'   shiny::shinyApp(ui, server)
#' }
NULL


#' @rdname navbarWidget
#' @export
navbarWidgetInput <- function(id, addUserInfo = TRUE) {

  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))

  shiny::tagList(
    shinyalert::useShinyalert(),
    rapbase::appNavbarUserWidget(
      user = shiny::uiOutput(shiny::NS(id, "name")),
      organization = shiny::uiOutput(shiny::NS(id, "affiliation")),
      addUserInfo = addUserInfo,
      namespace = id
    ),
    shiny::tags$head(
      shiny::tags$link(rel = "shortcut icon", href = "rap/favicon.ico")
    )
  )
}

#' @rdname navbarWidget
#' @export
navbarWidgetServer <- function(id, orgName,
                               caller = environmentName(rlang::caller_env())) {

  shiny::moduleServer(id, function(input, output, session) {

    output$name <- shiny::renderText(rapbase::getUserFullName(session))
    output$affiliation <- shiny::renderText(
      paste(orgName, rapbase::getUserRole(session), sep = ", ")
    )

    # User info in widget
    userInfo <- rapbase::howWeDealWithPersonalData(session, callerPkg = caller)
    shiny::observeEvent(input$userInfo, {
      shinyalert::shinyalert(
        "Dette vet Rapporteket om deg:",
        userInfo,
        type = "", imageUrl = "rap/logo.svg",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        confirmButtonText = rapbase::noOptOutOk()
      )
    })
  })
}

#' @rdname navbarWidget
#' @export
navbarWidgetApp <- function(orgName = "Org Name") {

  ui <- shiny::tagList(
    shiny::navbarPage(
      "Testpage",
      shiny::tabPanel(
        "Testpanel",
        shiny::mainPanel(
        navbarWidgetInput("testWidget")
        )
      )
    )
  )
  server <- function(input, output, session) {
    navbarWidgetServer("testWidget", orgName = orgName)
  }

  shiny::shinyApp(ui, server)
}
