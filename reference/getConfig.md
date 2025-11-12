# Get configuration for package, if any

Try to obtain yaml-formatted configuration placed either as given by the
environment variable R_RAP_CONFIG_PATH or as provided by the package
itself. If none can be found the function exits with an error

## Usage

``` r
getConfig(fileName, packageName = "rapbase")
```

## Arguments

- fileName:

  String providing configuration file base name

- packageName:

  String providing the package name

## Value

A list of (yaml) configuration
