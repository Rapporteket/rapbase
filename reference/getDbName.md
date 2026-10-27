# Get database name

Get database name

## Usage

``` r
getDbName(dbName = "data")
```

## Arguments

- dbName:

  String providing the name of the database to connect to. If it is
  "data" it will use the MYSQL_DB_DATA environment variable, if it is
  "autoreport" it will use the MYSQL_DB_AUTOREPORT environment variable,
  and if it is "raplog" it will use the MYSQL_DB_LOG environment
  variable. If none of these are set, it will use the name provided.

## Value

String providing the name of the database.
