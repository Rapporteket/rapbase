# Read log entries

Internal function that provide log entries

## Usage

``` r
readLog(type, name = "", app_id = NULL)
```

## Arguments

- type:

  Character string defining which log to request data from. Must be one
  of `c("app", "report")`.

- name:

  Character string with registry filter. Default value is an empty
  string that will return all log entries. If not empty its value must
  correspond to an existing registry (*i.e.* R package) name.

- app_id:

  An identifier for a particular registry. Default value is NULL, in
  which case no action is taken. If value is provided, the log is
  filtered to show only entries matching chosen app_id.

## Value

A data frame of log entries
