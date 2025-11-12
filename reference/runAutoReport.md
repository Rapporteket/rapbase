# Run reports as defined in yaml config and ship content by email

Usually to be called by a scheduler, e.g. cron. If the provided day of
year matches those of the config the report is run as otherwise
specified in config. Functions called upon are expected to return a
character string providing a path to a file that can be attached to an
email or, in case of a bulletin, the email body itself. For bulletins,
files cannot be attached. The email itself is prepared and sent to
recipients defined in the config

## Usage

``` r
runAutoReport(
  dayNumber = as.POSIXlt(Sys.Date())$yday + 1,
  dato = Sys.Date(),
  group = NULL,
  type = c("subscription", "dispatchment"),
  dryRun = FALSE
)
```

## Arguments

- dayNumber:

  Integer day of year where January 1st is 1. Defaults to current day,
  *i.e.* `as.POSIXlt(Sys.Date())$yday + 1` (POSIXlt yday is base 0)

- dato:

  Date-class date when report will be run first time. Default value is
  set to [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)

- group:

  Character string defining the registry, normally corresponding to the
  R package name and the value stemming from the SHINYPROXY_GROUPS
  environment variable. Introduced as a new argument when running apps
  inside containers. Default value is set to
  [`rapbase::getUserGroups()`](userAttribute.md) to allow backward
  compatibility.

- type:

  Character vector defining the type of reports to be processed. May
  contain one or more of
  `c("subscription", "dispatchment", "bulletin")`. Defaults value set to
  `c("subscription", "dispatchment")`.

- dryRun:

  Logical defining if emails are to be sent. If TRUE a message with
  reference to the payload file is given but no emails will actually be
  sent. Default is FALSE

## Value

Emails with corresponding file attachment. If dryRun == TRUE just a
message

## Examples

``` r
# \donttest{
# Example depend on environment variable R_RAP_CONFIG_PATH being set
try(runAutoReport())
#> Error in getDbConfig(dbName) : 
#>   Could not connect to database because the enviroment
#>        variables MYSQL_HOST, MYSQL_USER and/or MYSQL_PASSWORD
#>        are not defined. Please check configuration.
# }
```
