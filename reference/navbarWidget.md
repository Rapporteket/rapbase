# Shiny modules providing GUI and server logic for user info widget

Shiny modules for making a user information widget in registry shiny
apps at Rapporteket. One benefit using these modules will be reduced
complexity and number of code lines for each registry.

## Usage

``` r
navbarWidgetInput(id, addUserInfo = TRUE, selectOrganization = FALSE)

navbarWidgetServer(id, orgName, ...)

navbarWidgetServer2(id, orgName, map_orgname = NULL, caller = NULL)

navbarWidgetApp(orgName = "Org Name")
```

## Arguments

- id:

  Character string providing module namespace

- addUserInfo:

  Logical defining if an "about" hyperlink is to be added

- selectOrganization:

  Logical providing option for selecting among available organizations
  and roles.

- orgName:

  Character string naming the organization

- ...:

  Further arguments, currently not used

- map_orgname:

  A data.frame containing two columns:

  UnitId

  :   unit ids

  orgname

  :   corresponding organization names

- caller:

  Character string naming the environment this function was called from.
  The value is used to display the current version of the R package
  representing the registry at Rapporteket.

## Value

Shiny objects, mostly. `navbarWidgetServer2()` invisibly returns a list
of reactive values representing user metadata and privileges. See
[`userAttribute`](userAttribute.md) for further details on these values.

## Details

These modules take use of the shiny session object to obtain data for
the widget. Hence, a Rapporteket like context will be needed for these
modules to function properly. For deployment of (shiny) application as
containers make sure to migrate to `navbarWidgetServer2()`. In addition
to serving the user information widget, this function provides a list of
reactive user attributes. Hence, when using `navbarWidgetServer2()` the
source of (static) user attributes is no longer the shiny session object
but rather the list object (of reactive user attributes) returned by
this function.

## Examples

``` r
## client user interface function
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

## server function
server <- function(input, output, session) {
  navbarWidgetServer("testWidget", orgName = "Test org")
}

## run the app in an interactive session and a Rapporteket like environment
if (interactive() && isRapContext()) {
  shiny::shinyApp(ui, server)
}
```
