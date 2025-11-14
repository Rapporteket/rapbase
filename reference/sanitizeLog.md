# Sanitize log entries that have reached end of life

Function that removes log entries older than a given number of days.

## Usage

``` r
sanitizeLog(eolDays = 730)
```

## Arguments

- eolDays:

  Number of days to keep log entries. Entries older than this will be
  removed. Default value is 730 days (2 years).

## Value

NULL on success
