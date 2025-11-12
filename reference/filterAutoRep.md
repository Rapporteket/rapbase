# Filter auto report data

Generic function to filter various entities from auto report data

## Usage

``` r
filterAutoRep(data, by, pass)
```

## Arguments

- data:

  List (nested) specifying auto reports to be filtered. May be obtained
  by `rapbase::getConfig(fileName = "autoReport.yml")`

- by:

  Character string defining the filtering entity and must be one of
  `c("package", "type", "owner", "organization")`. The term 'package'
  represents the registry name

- pass:

  Character vector defining the values of the filtering entity that will
  allow reports to pass through the filter

## Value

List of auto reports matching the filtering criteria

## Examples

``` r
ar <- data.frame(type = c("A", "B"))
filterAutoRep(ar, "type", "B") # ar2
#>   type
#> 1    B
```
