# Shiny app with database export functionality

Shiny app with database export functionality

## Usage

``` r
exportApp(teamName = "", dbName = "data", logAsJson = TRUE)
```

## Arguments

- teamName:

  Character string, corresponding to github team name

- dbName:

  Character string, can be used to specify name of database if needed.
  Defaults to "data", which will work for most registries.

- logAsJson:

  Logical, if TRUE (default) logging will be done in JSON format.
