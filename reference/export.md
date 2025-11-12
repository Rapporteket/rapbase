# Shiny modules providing GUI and server logic for Export

Functions for registries that wants to implement exporting of registry
databases, *e.g.* for local development purposes. Also includes relevant
helper functions

## Usage

``` r
exportUCInput(id)

exportUCServer(id, registryName, repoName = registryName, eligible = TRUE)

exportUCServer2(
  id,
  registryName,
  repoName = registryName,
  eligible = shiny::reactiveVal(TRUE)
)

exportUCApp(registryName = "rapbase")

selectListPubkey(pubkey)

exportDb(registryName, compress = FALSE, session)
```

## Arguments

- id:

  Character string module ID

- registryName:

  Character string registry name key

- repoName:

  Character string defining the github repository name of the registry.
  Default value is `registryName`.

- eligible:

  Logical defining if the module should be allowed to work at full
  capacity. This might be useful when access to module products should
  be restricted. Default is TRUE, *i.e.* no restrictions.

- pubkey:

  Character vector with public keys

- compress:

  Logical if export data is to be compressed (using gzip). FALSE by
  default.

- session:

  Shiny session object

## Value

Shiny objects, mostly. Helper functions may return other stuff too.

## Examples

``` r
## client user interface function
ui <- shiny::fluidPage(
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      exportUCInput("test"),
    ),
    shiny::mainPanel(
      NULL
    )
  )
)

## server function
server <- function(input, output, session) {
  exportUCServer("test", registryName = "rapbase")
}

## run the shiny app in an interactive environment
if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
