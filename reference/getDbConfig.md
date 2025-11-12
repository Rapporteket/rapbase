# Get database connection configuration

Get database connection configuration

## Usage

``` r
getDbConfig(dbName = "data", sqlite = FALSE)
```

## Arguments

- dbName:

  String providing the name of the database to connect to. If it is
  "data" it will use the MYSQL_DB_DATA environment variable, if it is
  "autoreport" it will use the MYSQL_DB_AUTOREPORT environment variable,
  and if it is "raplog" it will use the MYSQL_DB_LOG environment
  variable. If none of these are set, it will use the name provided.

- sqlite:

  A boolean indicating if the connection is to a SQLite database. If
  TRUE, the envicronment variables MYSQL_HOST, MYSQL_USER and
  MYSQL_PASSWORD are not needed.

## Value

A list with name, user, password and host of the db connection.
