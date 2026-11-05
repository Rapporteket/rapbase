# Stats Server Module with Reactive Eligibility

Server module for creating usage statistics with eligibility controlled
by a reactive value.

## Usage

``` r
statsServer2(
  id,
  registryName,
  app_id = NULL,
  eligible = shiny::reactiveVal(TRUE)
)
```

## Arguments

- id:

  Character string providing the Shiny module id.

- registryName:

  Character string with registry package name.

- app_id:

  Optional identifier used to filter log entries for a specific
  application.

- eligible:

  Reactive logical indicating whether the module should be available at
  full capacity. Default is shiny::reactiveVal(TRUE).

## Value

A Shiny server module.
