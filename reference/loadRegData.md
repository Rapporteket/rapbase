# Provider of data for registries at Rapporteket

Generic to registries, provide reporting data obtained from sql
databases Underlying this function is rapbase::RapporteketDbConnection

## Usage

``` r
loadRegData(registryName = "data", query, dbType = "mysql")

describeRegistryDb(registryName, tabs = c())

nlinesRegistryDb(registryName, tab = "")
```

## Arguments

- registryName:

  String providing the name of the database to connect to. If it is
  "data" it will use the MYSQL_DB_DATA environment variable, if it is
  "autoreport" it will use the MYSQL_DB_AUTOREPORT environment variable,
  and if it is "raplog" it will use the MYSQL_DB_LOG environment
  variable. If none of these are set, it will use the name provided.

- query:

  String SQL query to obtain the data

- dbType:

  String Type of db to query, currently "mysql" (default) and "mssql"

- tabs:

  Character vector for optional definition of tables to describe.
  Defaults to an empty vector in which case all tables are used

- tab:

  String name of the table for which to get number of lines

## Value

data frame containing registry data or a list with table names and
corresponding fields with attributes
