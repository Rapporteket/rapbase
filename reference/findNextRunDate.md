# Find next run date for automated reports

Find the next date that an automated report is supposed to be run.
Likely, this function will only be relevant for shiny apps when this
date is to be printed

## Usage

``` r
findNextRunDate(
  runDayOfYear,
  baseDayNum = as.POSIXlt(Sys.Date())$yday + 1,
  startDate,
  terminateDate,
  interval = NULL,
  returnFormat = "%A %e. %B %Y"
)
```

## Arguments

- runDayOfYear:

  Numeric vector providing year-day numbers

- baseDayNum:

  Numeric defining base year-day. Default is today

- startDate:

  Character string of format "YYYY-MM-DD" defining the date of the very
  first run. If set to NULL (default) or a none future date (compared to
  the date represented by `baseDayNum` for the current year) it will be
  disregarded.

- terminateDate:

  Date-class date after which report is no longer run. Default value set
  to `NULL` in which case the function will provide an expiry date
  adding 3 years to the current date if in a PRODUCTION context and 1
  month if not

- interval:

  String defining a time interval as defined in
  [`seq.POSIXt`](https://rdrr.io/r/base/seq.POSIXt.html). Default value
  is an empty string

- returnFormat:

  String providing return format as described in
  [`strptime`](https://rdrr.io/r/base/strptime.html) in the current
  locale. Defaults to "%A %d. %B %Y" that will provide something like
  'Mandag 20. januar 2019' in a Norwegian locale

## Value

String date for printing

## Examples

``` r
# Will return Jan 30 in the current year and locale with default formatting
findNextRunDate(runDayOfYear = c(10, 20, 30),
 baseDayNum = 20, startDate = 1, terminateDate = 50)
#> [1] "Saturday 21. February 1970"
```
