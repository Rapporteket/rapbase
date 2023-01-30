#' Shiny modules providing GUI and server logic for user info widget
#'
#' Shiny modules for making a user information widget in registry shiny apps at
#' Rapporteket. One benefit using these modules will be reduced complexity and
#' number of code lines for each registry.
#'
#' These modules take use of the shiny session object to obtain data for the
#' widget. Hence, a Rapporteket like context will be needed for these modules to
#' function properly. For deployment of (shiny) application as containers make
#' sure to migrate to \code{navbarWidgetServer2()}. In addition to serving the
#' user information widget, this function provides a list of reactive user
#' attributes. Hence, when using \code{navbarWidgetServer2()} the source of
#' (static) user attributes is no longer the shiny session object but rather the
#' list object (of reactive user attributes) returned by this function.
#'
#' @param id Character string providing module namespace
#' @param addUserInfo Logical defining if an "about" hyperlink is to be added
#' @param selectOrganization Logical providing option for selecting among
#'   available organizations and roles.
#' @param orgName Character string naming the organization
#' @param caller Character string naming the environment this function was
#'   called from. Default value is
#'   \code{environmentName(topenv(parent.frame()))}. The value is used to
#'   display the current version of the R package representing the registry at
#'   Rapporteket. If this module is called from exported functions in the
#'   registry R package the default value should be applied. If the module is
#'   called from outside the registry environment \code{caller} must be set to
#'   the actual name of the R package.
#'
#' @return Shiny objects, mostly.  \code{navbarWidgetServer2()} invisibly returns
#'   a list of reactive values representing user metadata and privileges. See
#'   \code{\link{userAttribute}} for further details on these values.
#' @name navbarWidget
#' @aliases navbarWidgetInput navbarWidgetServer navbarWidgetServer2
#'   navbarWidgetApp
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
navbarWidgetInput <- function(id,
                              addUserInfo = TRUE,
                              selectOrganization = FALSE) {
  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))

  shiny::tagList(
    appNavbarUserWidget(
      user = shiny::uiOutput(shiny::NS(id, "name")),
      organization = shiny::uiOutput(shiny::NS(id, "affiliation")),
      addUserInfo = addUserInfo,
      selectOrganization = selectOrganization,
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
navbarWidgetServer2 <- function(
    id,
    orgName,
    caller = environmentName(topenv(parent.frame()))
) {

  shiny::moduleServer(id, function(input, output, session) {

    user <- userAttribute(caller)
    stopifnot(length(user$name) > 0)

    # Initial privileges and affiliation will be first in list
    rv <- shiny::reactiveValues(
      name = user$name[1],
      fullName = user$fullName[1],
      phone = user$phone[1],
      email = user$email[1],
      group = user$group[1],
      unit = user$unit[1],
      org = user$org[1],
      role = user$role[1],
      orgName = user$orgName[1]
    )

    output$name <- shiny::renderText(rv$fullName)
    output$affiliation <- shiny::renderText(paste(orgName, rv$role, sep = ", "))

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

    # Select organization in widget (for container apps only)
    shiny::observeEvent(input$selectOrganization, {
      choices <- user$unit
      names(choices) <- paste0(
        user$orgName, " (", user$org, ") - ", user$role
      )

      shinyalert::shinyalert(
        html = TRUE,
        title = "Velg organisasjon og rolle",
        text = shiny::tagList(shiny::tagList(
          shiny::p(
            paste(
              "Velg organisasjon og rolle du \u00f8nsker \u00e5 representere",
              "for", orgName, "i Rapporteket og trykk OK.",
              "Dine valgmuligheter er basert p\u00e5 de tilganger som er satt.",
              "Ta kontakt med registeret om du mener at lista over valg",
              "ikke er riktg."
            )
          ),
          shiny::selectInput(
            session$ns("unit"),
            "",
            choices,
            selected = rv$unit
          )
        )),
        type = "", imageUrl = "rap/logo.svg",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        confirmButtonText = "OK"
      )
    })

    shiny::observeEvent(input$unit, {
      rv$name <- user$name[user$unit == input$unit]
      rv$fullName <- user$fullName[user$unit == input$unit]
      rv$phone <- user$phone[user$unit == input$unit]
      rv$email <- user$email[user$unit == input$unit]
      rv$group <- user$group[user$unit == input$unit]
      rv$unit <- user$unit[user$unit == input$unit]
      rv$org <- user$org[user$unit == input$unit]
      rv$role <- user$role[user$unit == input$unit]
      rv$orgName <- user$orgName[user$unit == input$unit]
    })

    invisible(
      list(
        name = shiny::reactive(rv$name),
        fullName = shiny::reactive(rv$fullName),
        phone = shiny::reactive(rv$phone),
        email = shiny::reactive(rv$email),
        group = shiny::reactive(rv$group),
        unit = shiny::reactive(rv$unit),
        org = shiny::reactive(rv$org),
        role = shiny::reactive(rv$role),
        orgName = shiny::reactive(rv$orgName)
      )
    )
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
          navbarWidgetInput(
            "testWidget",
            addUserInfo = TRUE,
            selectOrganization = FALSE
          )
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
#'   of the widget (TRUE) or not (FALSE, default)
#' @param selectOrganization Logical if organization can be selected.
#' @param namespace Character string providing the namespace to use, if any.
#'   Defaults is \code{NULL} in which case no namespace will be applied.
#'
#' @return Ready made html script
#' @export
#'
#' @examples
#' appNavbarUserWidget()
appNavbarUserWidget <- function(user = "Undefined person",
                                organization = "Undefined organization",
                                addUserInfo = FALSE,
                                selectOrganization = FALSE,
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

  if (selectOrganization) {
    org <- shiny::tags$a(
      id = shiny::NS(namespace, "selectOrganization"),
      href = "#",
      class = "action-button",
      shiny::HTML(gsub("\\n", "", organization))
    )
  } else {
    org <- organization
  }

  txtWidget <-
    paste0(
      "var header = $('.navbar> .container-fluid');\n",
      "header.append('<div class=\"navbar-brand\" ",
      "style=\"float:right;vertical-align:super;font-size:65%\">",
      userInfo,
      user,
      org,
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
