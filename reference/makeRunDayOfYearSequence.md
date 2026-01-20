# Make a sequence of day numbers from av given date and interval

This function provides an even sequence of day numbers spanning 365/366
days from the start date and interval provided. Mainly to be used in
setting up automated reports at Rapporteket

## Usage

``` r
makeRunDayOfYearSequence(startDay = Sys.Date(), interval)
```

## Arguments

- startDay:

  Start date of sequence. May be provided as a string, *e.g.*
  \\2019-03-17\\ or as class \\Date\\. Defaults to today

- interval:

  String representing a valid seq.POSIXt interval such as "DSTday",
  "week", "month", "quarter" or "year")

## Value

Integer vector of day numbers

## Examples

``` r
makeRunDayOfYearSequence(interval = "month")
#>  [1]  20  51  79 110 140 171 201 232 263 293 324 354
```
