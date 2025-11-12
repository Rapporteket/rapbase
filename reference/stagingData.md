# Staging data functions

Low level functions for handling registry staging data at Rapporteket.
As such, these functions does not provide staging data management *per
se*. Proper management, *e.g.* staging data updates and fallback logic
must therefore be established within each registry that take staging
data into use.

## Usage

``` r
listStagingData(registryName, dbTable = "data")

mtimeStagingData(registryName, dbTable = "data")

saveStagingData(registryName, dataName, data, dbTable = "data")

loadStagingData(registryName, dataName, dbTable = "data")

deleteStagingData(registryName, dataName, dbTable = "data")

cleanStagingData(eolAge, dryRun = TRUE, dbTable = "data")
```

## Arguments

- registryName:

  Character string providing the registry name.

- dbTable:

  Character string providing the database table name to be used

- dataName:

  Character string providing the data set name.

- data:

  A data object such as a data.frame to be stored as `dataName`.

- eolAge:

  Numeric providing the staging data end-of-life age in seconds. Based
  on the current time and the time of storage staging files older than
  `eolAge` will be identified as subject for removal.

- dryRun:

  Logical defining if function is to be run in dry (none destructive)
  mode.

## Value

- `listStagingData()` returns a character vector of staging data sets
  for the given registry (`registryName`).

- `mtimeStagingData()` returns a staging data set named POSIXct vector
  of modification times for the given registry (`registryName`).

- `saveStagingData()` when successful returns the data object (`data`),
  invisibly. If saving fails a warning is issued and the function
  returns FALSE.

- `loadStagingData()` returns the data object corresponding to the name
  given upon saving (`dataName`). If the requested data set for loading
  does not exist the function returns FALSE.

- `deleteStagingData()` returns TRUE if the data set was deleted and
  FALSE if not.

- `cleanStagingData()` returns a list of data sets (to be) removed.

## Details

Staging data is stored as binary large objects in a database. A per
registry symmetric encryption of storage content is enforced. Keys used
for encryption are generated from existing database credentials.
Therefore, please note that removing or changing such credentials will
render any historic staging data inaccessible.

`cleanStagingData()` globally removes all staging data with store date
prior to the end-of-life age provided. This is a vastly destructive
function that should be used with great care.

## Examples

``` r
# \donttest{
## Prep test data
registryName <- "rapbase"
dataName <- "testData"
data <- mtcars

## Save data for staging
try(saveStagingData(registryName, dataName, data))
#> Error in getDbConfig(key) : 
#>   Could not connect to database because the enviroment
#>        variables MYSQL_HOST, MYSQL_USER and/or MYSQL_PASSWORD
#>        are not defined. Please check configuration.

## List data currently in staging
try(listStagingData(registryName))
#> Error in getDbConfig(key) : 
#>   Could not connect to database because the enviroment
#>        variables MYSQL_HOST, MYSQL_USER and/or MYSQL_PASSWORD
#>        are not defined. Please check configuration.

## Retrieve data set from staging and compare to outset
try(stagedData <- loadStagingData(registryName, dataName))
#> Error in getDbConfig(key) : 
#>   Could not connect to database because the enviroment
#>        variables MYSQL_HOST, MYSQL_USER and/or MYSQL_PASSWORD
#>        are not defined. Please check configuration.
try(identical(data, stagedData))
#> Error in eval(expr, envir) : object 'stagedData' not found

## Get modification time for staging file(s)
try(mtimeStagingData(registryName))
#> Error in getDbConfig(key) : 
#>   Could not connect to database because the enviroment
#>        variables MYSQL_HOST, MYSQL_USER and/or MYSQL_PASSWORD
#>        are not defined. Please check configuration.
# }
```
