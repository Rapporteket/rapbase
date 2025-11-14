# Append a log record

Internal function adding a record to the log.

## Usage

``` r
appendLog(event, name)
```

## Arguments

- event:

  data.frame of one record holding the fields of whatever that is to be
  logged.

- name:

  String defining the name of the log, currently one of "appLog" or
  "reportLog".

## Value

Provides a new record in the log database. The database have to be set
up in advance.
