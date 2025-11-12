# Write automated report metadata

Write automated report metadata

## Usage

``` r
writeAutoReportData(config)
```

## Arguments

- config:

  a list of yaml configuration

## Examples

``` r
# \donttest{
# Example depend on environment variable R_RAP_CONFIG_PATH being set
try(config <- readAutoReportData())
#> Error in getDbConfig(dbName) : 
#>   Could not connect to database because the enviroment
#>        variables MYSQL_HOST, MYSQL_USER and/or MYSQL_PASSWORD
#>        are not defined. Please check configuration.
try(writeAutoReportData(config = config))
#> Error in eval(expr, envir) : object 'config' not found
# }
```
