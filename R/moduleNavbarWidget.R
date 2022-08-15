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
#' @param addUserInfo Logical defining if an "about" hyperlink is to be added
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
    appNavbarUserWidget(
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
      paste(orgName, getUserRole(session), sep = ", ")
    )

    # User info in widget
    userInfo <- howWeDealWithPersonalData(session, callerPkg = caller)
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


#' Create widget for registry apps at Rapporteket
#'
#' Provides a widget-like information and utility block to be applied to all
#' registry apps at Rapporteket. Contains the user name, organization and
#' logout/exit as hyperlinked text.
#'
#' Normally, user information will be be provided through the session parameter
#' and hence this will have to be provided from the server. The "rendering" of
#' this info must hence be done within a layout element at the client such as
#' a \code{tabPanel}. Selecting any one of them should be fine... At the
#' client, both \code{uiOutput} and \code{textOutput} will be fine "rendering
#' the information provided by the server.
#'
#' Example of use in shiny (pseudo code):
#' \preformatted{
#' server <- function(input, output, session) {
#'   ...
#'   output$appUserName <- renderText(getUserName(session))
#'   output$appUserOrg <- renderText(getUserReshId(session))
#'   ...
#' }
#'
#' ui <- tagList(
#'   navbarPage(
#'     ...,
#'     tabPanel(...,
#'     appNavbarUserWidget(user = uiOutput(appUserName),
#'     organization = textOutput(appUserOrg))
#'     ),
#'     ...
#'   )
#' )
#' }
#'
#' @param user String providing the name of the user
#' @param organization String providing the organization of the user
#' @param addUserInfo Logical defining whether a user data pop-up is to be part
#' of the widget (TRUE) or not (FALSE, default)
#' @param namespace Character string providing the namespace to use, if any.
#' Defaults is \code{NULL} in which case no namespace will be applied.
#'
#' @return Ready made html script
#' @export
#'
#' @examples
#' appNavbarUserWidget()
appNavbarUserWidget <- function(user = "Undefined person",
                                organization = "Undefined organization",
                                addUserInfo = FALSE,
                                namespace = NULL) {
  if (addUserInfo) {
    userInfo <- shiny::tags$a(
      id = shiny::NS(namespace, "userInfo"),
      href = "#",
      class = "action-button",
      "Om:"
    )
  } else {
    userInfo <- character()
  }

  txtWidget <-
    paste0(
      "var header = $('.navbar> .container-fluid');\n",
      "header.append('<div class=\"navbar-brand\" ",
      "style=\"float:right;vertical-align:super;font-size:65%\">",
      userInfo,
      user,
      organization,
      "</div>');\n",
      "console.log(header)"
    )

  shiny::tags$script(shiny::HTML(txtWidget))
}


#' Render text in pop-up
#'
#' Render text on how Rapporteket deals with personal data
#'
#' @param session A shiny session object used to personalize the text
#' @param callerPkg Character string naming the package that makes a call to
#' this function in case version number of the caller package should be added
#' to the returned (html) info text. Default to NULL in which case no version
#' number for the caller will be added to the info text
#'
#' @return fragment html info text
#' @export

howWeDealWithPersonalData <- function(session, callerPkg = NULL) {
  pkg <- list()
  pkg$name <- as.vector(utils::installed.packages()[, 1])
  pkg$ver <- as.vector(utils::installed.packages()[, 3])

  pkgs <- intersect(c("shiny", "rapbase"), pkg$name)

  if (!is.null(callerPkg)) {
    if (callerPkg %in% pkg$name) {
      pkgs <- c(pkgs, callerPkg)
    } else {
      warning(paste(callerPkg, "is not an installed package."))
    }
  }

  ind <- pkg$name %in% pkgs
  pkgs <- pkg$name[ind]
  vers <- pkg$ver[ind]

  # add R itself
  pkgs <- c("R", pkgs)
  vers <- c(paste(R.version$major, R.version$minor, sep = "."), vers)

  pkgInfo <- paste0(pkgs, vers, collapse = ", ")

  sourceFile <- system.file(
    "howWeDealWithPersonalData.Rmd",
    package = "rapbase"
  )

  renderRmd(
    sourceFile = sourceFile, outputType = "html_fragment", params = list(
      session = session,
      pkgInfo = pkgInfo
    )
  )
}
