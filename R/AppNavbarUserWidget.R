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
#'     appNavbarUserWidget(user = uiOutput(appUserName), organization = textOutput(appUserOrg))
#'     ),
#'     ...
#'   )
#' )
#' }
#' 
#' @param user String providing the name of the user
#' @param organization String providing the organization of the user
#'
#' @return Ready made html script
#' @export
#'
#' @examples
#' appNavbarUserWidget()

appNavbarUserWidget <- function(user = "Undefined person", organization = "Undefined organization") {
  
  txtWidget <-
    paste0("var header = $('.navbar> .container-fluid');\n",
           "header.append('<div class=\"navbar-brand\" style=\"float:right;font-size:75%\">",
           user,
           organization,
           "<a href=\"http://localhost:80\">Logg ut</a>",
           "</div>');\n",
           "console.log(header)")
  
  shiny::tags$script(shiny::HTML(txtWidget))
}