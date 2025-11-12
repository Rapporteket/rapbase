# Shiny modules and helper functions for registry usage reports

These modules may be used by registries for easy setup of usage reports.
The intended purpose is to provide registry staff access to when and by
whom the resources at Rapporteket were used, *i.e.* application start-up
and single report usage. As such, this will be a tool to provide useful
statistics. However, it might also serve as a formal monitor utility but
only if logging is carefully implemented throughout the relevant
functions that make up the registry application at Rapporteket.

## Usage

``` r
statsInput(id)

statsUI(id)

statsServer(id, registryName, app_id = NULL, eligible = TRUE)

statsServer2(
  id,
  registryName,
  app_id = NULL,
  eligible = shiny::reactiveVal(TRUE)
)

statsApp()

logFormat(log)

logTimeFrame(log, startDate, endDate)
```

## Arguments

- id:

  Character string shiny module id

- registryName:

  Character string registry name key

- app_id:

  An identifier for a particular registry. Default value is NULL, in
  which case no action is taken. If value is provided, the log is
  filtered to show only entries matching chosen app_id.

- eligible:

  Logical defining if the module should be allowed to work at full
  capacity. This might be useful when access to module products should
  be restricted. Default is TRUE, *i.e.* no restrictions.

- log:

  Data frame containing log data (in Rapporteket format)

- startDate:

  Date object defining start of interval (character representation
  "YYYY-MM-DD")

- endDate:

  Date object defining end of interval (character representation
  "YYYY-MM-DD")

## Value

Shiny objects, mostly. Helper functions may return other stuff too.

## Examples

``` r
# client user interface function
ui <- shiny::fluidPage(
  shiny::sidebarLayout(
    shiny::sidebarPanel(statsInput("test")),
    shiny::mainPanel(statsUI("test"))
  )
)

# server function
server <- function(input, output, session) {
  statsServer("test", registryName = "rapbase", eligible = TRUE)
}

# run the shiny app in an interactive environment
if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
