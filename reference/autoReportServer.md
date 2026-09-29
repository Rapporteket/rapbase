# Auto Report Server Module

Creates and manages automated report subscriptions, dispatchments, and
bulletins.

## Usage

``` r
autoReportServer(
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
)
```

## Arguments

- id:

  Character string providing the shiny module id.

- registryName:

  Character string with registry package name.

- type:

  One of \`"subscription"\`, \`"dispatchment"\` or \`"bulletin"\`.

- org:

  Reactive organization id.

- paramNames:

  Reactive vector of parameter names.

- paramValues:

  Reactive vector of parameter values.

- reports:

  Report metadata list.

- orgs:

  Named list of organizations.

- eligible:

  Reactive logical indicating module availability.

- freq:

  Default report frequency.

- user:

  User metadata reactives.

- runAutoReportButton:

  Logical indicating if testing button should be shown.

## Value

A Shiny server module.

## Details

The \`reports\` argument must be a list where each entry represents one
report configuration:

- synopsis:

  Description of the report.

- fun:

  Exported report function name.

- paramNames:

  Function argument names.

- paramValues:

  Corresponding argument values.

## Examples

``` r
## make a list for report metadata
reports <- list(
  FirstReport = list(
    synopsis = "First example report",
    fun = "fun1",
    paramNames = c("organization", "topic", "outputFormat"),
    paramValues = c(111111, "work", "html")
  ),
  SecondReport = list(
    synopsis = "Second example report",
    fun = "fun2",
    paramNames = c("organization", "topic", "outputFormat"),
    paramValues = c(111111, "leisure", "pdf")
  )
)

## make a list of organization names and numbers
orgs <- list(
  OrgOne = 111111,
  OrgTwo = 222222
)

## client user interface function
ui <- shiny::fluidPage(
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      autoReportFormatInput("test"),
      autoReportOrgInput("test"),
      autoReportInput("test")
    ),
    shiny::mainPanel(
      autoReportUI("test")
    )
  )
)

## server function
server <- function(input, output, session) {
  org <- autoReportOrgServer("test", orgs)
  format <- autoReportFormatServer("test")

  # set reactive parameters overriding those in the reports list
  paramNames <- shiny::reactive(c("organization", "outputFormat"))
  paramValues <- shiny::reactive(c(org$value(), format()))

  autoReportServer(
    id = "test",
    registryName = "rapbase",
    type = "dispatchment",
    org = org$value,
    paramNames = paramNames,
    paramValues = paramValues,
    reports = reports,
    orgs = orgs,
    eligible = shiny::reactiveVal(TRUE),
    freq = "month",
    user = user
  )
}

# run the shiny app in an interactive environment
if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
