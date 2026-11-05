# Stats Server Module

Server module for creating usage statistics based on log data.

## Usage

``` r
statsServer(id, registryName, app_id = NULL, eligible = TRUE)
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

  Logical indicating whether the module should be available at full
  capacity. Default is TRUE.

## Value

A Shiny server module.
