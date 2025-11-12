# Rapporteket context

Call to this function will return TRUE when run on a system where the
environment variable `R_RAP_INSTANCE` is set to either "DEV", "TEST",
"QA" or "PRODUCTION" and FALSE otherwise

## Usage

``` r
isRapContext()
```

## Value

Logical if system has a defined Rapporteket context

## Examples

``` r
isRapContext()
#> [1] FALSE
```
