# Create and add report to config

Adds an entry to the system configuration of reports to run at given
intervals. After generating the configuration from the new entry the
function load the current system configuration, adds the new entry and
saves the updated system configuration.

## Usage

``` r
createAutoReport(
  synopsis,
  package,
  type = "subscription",
  fun,
  paramNames,
  paramValues,
  owner,
  ownerName = "",
  email,
  organization,
  runDayOfYear,
  startDate = as.character(Sys.Date()),
  terminateDate = NULL,
  interval = "",
  intervalName = "",
  dryRun = FALSE
)
```

## Arguments

- synopsis:

  String with description of the report and to be used in subject field
  of email distributed reports

- package:

  String with package name also corresponding to registry

- type:

  Character string defining type of auto report. Currently, one of
  'subscription' (default) or 'dispatchment'

- fun:

  String providing name of function to be called for generating report

- paramNames:

  String vector where each element corresponds to the input parameter to
  be used in the above function

- paramValues:

  String vector with corresponding values to paramNames

- owner:

  String providing the owner of the report. Usually a user name

- ownerName:

  String providing full name of owner. Defaults to an empty string to
  maintain backwards compatibility

- email:

  String with email address to recipient of email containing the report

- organization:

  String identifying the organization the owner belongs to

- runDayOfYear:

  Integer vector with day numbers of the year when the report is to be
  run

- startDate:

  Date-class date when report will be run first time. Default value is
  set to `Sys.Date() + 1` *i.e.* tomorrow.

- terminateDate:

  Date-class date after which report is no longer run. Default value set
  to `NULL` in which case the function will provide an expiry date
  adding 3 years to the current date if in a PRODUCTION context and 1
  month if not

- interval:

  String defining a time interval as defined in
  [`seq.POSIXt`](https://rdrr.io/r/base/seq.POSIXt.html). Default value
  is an empty string

- intervalName:

  String providing a human understandable representation of `interval`.
  Default value is an empty string

- dryRun:

  Logical defining if global auto report config actually is to be
  updated. If set to TRUE the actual config (all of it) will be returned
  by the function. FALSE by default

## Value

Nothing unless dryRun is set TRUE in which case a list of all config
will be returned

## See also

[`deleteAutoReport`](deleteAutoReport.md)
