# Kick off functions at Rapporteket

This function will normally be executed by a cron daemon. Once started
this function will nest through schedule functions defined in a
configuration file, *e.g.* "rapbaseConfig.yml".

## Usage

``` r
fireInTheHole(flipPeriod = FALSE)
```

## Arguments

- flipPeriod:

  Logical only used for testing. FALSE by default

## Details

This is a crontab example running fireInTheHole() every night at 01
hours, Monday through Friday and with emails suppressed:

    0  1 * * 1-5 Rscript -e 'rapbase::fireInTheHole()' >/dev/null
    2>&1

## Examples

``` r
# \donttest{
# Depends on the env var R_RAP_CONFIG_PATH being properly set
try(fireInTheHole())
#> Error in if (hour >= conf$r$schedule$nocturnal$startHour && hour < conf$r$schedule$nocturnal$endHour) { : 
#>   missing value where TRUE/FALSE needed
# }
```
