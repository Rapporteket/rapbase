# Provide connection handle for data source at Rapporteket

Generic to registries, handle the data source connections, including
usernames and passwords needed to open these connections

## Usage

``` r
rapOpenDbConnection(dbName, dbType = "mysql")
```

## Arguments

- dbName:

  String providing the name of the database to connect to. If it is
  "data" it will use the MYSQL_DB_DATA environment variable, if it is
  "autoreport" it will use the MYSQL_DB_AUTOREPORT environment variable,
  and if it is "raplog" it will use the MYSQL_DB_LOG environment
  variable. If none of these are set, it will use the name provided.

- dbType:

  String providing type of data source, one of "mysql" and "sqlite".
  Defaults to "mysql". "mssql" is not supported anymore.

## Value

A named list of con and drv representing the db connection handle and
driver, respectively.
