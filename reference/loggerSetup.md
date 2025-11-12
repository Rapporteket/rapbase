# Settings for logging as json

Every info, warning and error will be logged in json format.

## Usage

``` r
loggerSetup(
  usernameEnv = "SHINYPROXY_USERNAME",
  appidEnv = "SHINYPROXY_APPID",
  hooks = TRUE
)
```

## Arguments

- usernameEnv:

  Global variable containing user name

- appidEnv:

  Global variable containing application name

- hooks:

  Logical defining if hooks for automatic logging should be set
