# Simple test of automated report

Simple test of automated reporting from definitions provided in a yaml
config file

## Usage

``` r
.testAutoReport(aNum = 1, aChar = "a", anExp = Sys.Date(), bulletin = 0)
```

## Arguments

- aNum:

  a number

- aChar:

  a character

- anExp:

  an expression

- bulletin:

  Integer defining if report is of type bulletin (1) or not (0). Set to
  0 by default

## Value

A simple message listing the contents of the arguments

## Examples

``` r
.testAutoReport()
#> [1] "/tmp/RtmpGdOGdv/file1b6924382513.txt"
```
