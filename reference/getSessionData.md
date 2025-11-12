# Get session data

Internal function providing session data relevant to logging.

## Usage

``` r
getSessionData(group = NULL)
```

## Arguments

- group:

  Character string providing the name of the app R package name. The
  term "group" is used to relate to the environmental variable
  SHINYPROXY_USERGROUPS that corresponds to the apps a given user can
  access.

## Value

A list of relevant log fields
