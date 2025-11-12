# Shiny modules providing the Stats Guide

Shiny modules providing the Stats Guide

## Usage

``` r
statsGuideUI(id)

statsGuideServer(id, registryName)

statsGuideApp()
```

## Arguments

- id:

  Character string module ID

- registryName:

  Character string registry name key

## Value

Functions ui and server representing the (module) app

## Examples

``` r
ui <- shiny::fluidPage(
  statsGuideUI("statsGuide")
)

server <- function(input, output, session) {
  statsGuideServer("statsGuide", "test")
}

if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
