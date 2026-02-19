# Shiny modules providing GUI and server logic for Export

Functions for registries that wants to implement exporting of registry
databases, *e.g.* for local development purposes. Also includes relevant
helper functions

## Usage

``` r
exportUCInput(id)

exportUCServer(
  id,
  dbName,
  teamName = dbName,
  eligible = shiny::reactiveVal(TRUE)
)

exportUCServer2(id, dbName, teamName, eligible = shiny::reactiveVal(TRUE))

exportUCApp(dbName = "rapbase")

selectListPubkey(pubkey)

exportDb(dbName, compress = FALSE, session)
```

## Arguments

- id:

  Character string module ID

- dbName:

  Character string database name. If this is \`data\`, then the database
  name is taken from the environment variable \`MYSQL_DB_DATA\`.

- teamName:

  Character string defining the github team name containing members
  allowed to export the database. Default value is `dbName`.

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
  exportUCServer("test", dbName = "rapbase")
}

## run the shiny app in an interactive environment
if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
