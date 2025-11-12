# Shiny modules and helper functions for registry auto reports

These shiny modules may be used to set up auto reporting from registries
at Rapporteket.

## Usage

``` r
autoReportUI(id)

autoReportOrgInput(id)

autoReportOrgServer(id, orgs)

autoReportFormatInput(id)

autoReportFormatServer(id)

autoReportInput(id)

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

autoReportServer2(...)
```

## Arguments

- id:

  Character string providing the shiny module id.

- orgs:

  Named list of organizations (names) and ids (values). When set to
  `NULL` (default) the ids found in auto report data will be used in the
  table listing existing auto reports.

- registryName:

  Character string with the registry name key. Must correspond to the
  registry R package name.

- type:

  Character string defining the type of auto reports. Must be one of
  `c("subscription", "dispatchment", "bulletin")`

- org:

  Shiny reactive or NULL (default) defining the organization (id) of the
  data source used for dispatchments and bulletins (in which case it
  cannot be set to NULL) and its value will be used to populate the
  *organization* field in auto report data (autoReport.yml) for these
  auto report types. On the other hand, since subscriptions are personal
  (per user) the only relevant organization id will implicit be that of
  the user and in this case any value of `org` will be disregarded.

- paramNames:

  Shiny reactive value as a vector of parameter names of which values
  are to be set interactively at application run time. Each element of
  this vector must match exactly those of `paramValues`. Default value
  is `shiny::reactiveVal("")`.

- paramValues:

  Shiny reactive value as a vector of those parameter values to be set
  interactively, *i.e.* as per user input in the application. Default
  value is set to `shiny::reactiveVal("")` in which case parameter
  values defined in `reports` will be used as is. In other words,
  explicit use of `paramValues` will only be needed if parameter values
  must be changed during application run time. If so, each element of
  this vector must correspond exactly to those of `paramNames`.

- reports:

  List of a given structure that provides meta data for the reports that
  are made available as automated reports. See Details for further
  description.

- eligible:

  Logical defining if the module should be allowed to work at full
  capacity. This might be useful when access to module products should
  be restricted. Default is TRUE, *i.e.* no restrictions.

- freq:

  Character string defining default frequency set in the auto report
  GUI. Must be one of `c("day", "week", "month", "quarter", "year")`.
  Default value is "month".

- user:

  List of shiny reactive values providing user metadata and privileges
  corresponding to the return value of
  [`navbarWidgetServer`](navbarWidget.md).

- runAutoReportButton:

  Logical defining if runAutoReport button should be made available in
  the GUI. Default is FALSE. If TRUE, a button will be made available to
  trigger running all auto reports for a given date. This is mainly
  useful for testing purposes.

- ...:

  Arguments passed to autoReportServer function

## Value

In general, shiny objects. In particular, `autoreportOrgServer` returns
a list with names "name" and "value" with corresponding reactive values
for the selected organization name and id. This may be used when
parameter values of auto report functions needs to be altered at
application run time.

## Details

The *reports* argument must be a list where each entry represents one
report and its name will be used in the auto report user interface for
selecting reports, *e.g.* `reports = list(MagicReport = ...)` will
produce the entry "MagicReport" in the GUI list of reports to select
from. The value of each entry must be another list with the following
names and values:

- synopsis:

  character string describing the report

- fun:

  report function base name (without"()")

- paramNames:

  character vector naming all arguments of *fun*

- paramValues:

  vector with values corresponding to *paramNames*

These named values will be used to run reports none-interactively on a
given schedule and must therefore represent existing and exported
functions from the registry R package. For subscriptions the *reports*
list can be used as is, more specifically that the values provided in
*paramValues* can go unchanged. It is likely that parameter values must
be set dynamically at runtime in which case *paramValues* must be a
reactive part of the application. See Examples on how function arguments
may be used as reactives in an application.

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

  autoReportServer2(
    id = "test", registryName = "rapbase", type = "dispatchment",
    org = org$value, paramNames = paramNames, paramValues = paramValues,
    reports = reports, orgs = orgs, eligible = TRUE, freq = "month", user
  )
}

# run the shiny app in an interactive environment
if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
