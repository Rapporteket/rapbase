# Shiny modules providing the Export Guide

Shiny modules providing the Export Guide

## Usage

``` r
exportGuideUI(id)

exportGuideServer(id, registryName)

exportGuideServer2(id, registryName)

exportGuideApp()
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
  exportGuideUI("exportGuide")
)

server <- function(input, output, session) {
  exportGuideServer("exportGuide", "test")
}

if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
