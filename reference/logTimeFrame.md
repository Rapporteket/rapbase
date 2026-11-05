# Filter Log by Date Interval

Keep only log entries between the supplied start and end dates.

## Usage

``` r
logTimeFrame(log, startDate, endDate)
```

## Arguments

- log:

  Data frame containing log data in Rapporteket format.

- startDate:

  Date object defining the start of the interval.

- endDate:

  Date object defining the end of the interval.

## Value

A filtered data frame with rows inside the interval.
