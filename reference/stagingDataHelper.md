# Data staging helper (internal) functions

A set of helper functions to aid staging of registry data at
Rapporteket.

## Usage

``` r
wrapStagingData(data)

unwrapStagingData(data)

dbStagingPrereq(key)

dbStagingConnection(key = NULL, con = NULL, init = FALSE)

dbStagingProcess(key, query, params = list(), statement = FALSE)
```

## Arguments

- data:

  A data object that is to be added to or collected from staging.

- key:

  Character string with key to be used for staging data store
  credentials.

- con:

  A database connection object.

- init:

  Logical defining if the function call will perform an initial set-up
  of a database. Default value is FALSE

- query:

  Character string providing a database query.

- params:

  List of values to be provided in a parameterized query.

- statement:

  Logical defining if a query is a statement or not. Default value is
  FALSE.

## Value

- `dbStagingPrereq()` ensures that a database for staging data is
  properly setup and returns a message, invisibly.

- `dbStagingConnection()` returns an open database connection object or,
  when an open connection object is provided as an argument, closes it
  and returns `NULL` invisibly.

- `dbStagingProcess()` returns the raw result of a database query based
  on the arguments provided.
