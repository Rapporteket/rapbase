# Display memory usage in the app

This function creates a reactive text output for displaying memory
usage. In your server function, assign it to `output$memory`:

## Usage

``` r
memoryDisplay(session = shiny::getDefaultReactiveDomain())
```

## Arguments

- session:

  Shiny session object.

## Value

A reactive text output displaying memory usage.

## Details

`output$memory <- memoryDisplay(session)`

The output ID `memory` is required and must match the textOutput ID used
in memoryTitle().

## Examples

``` r
    server <- function(input, output, session) {
      output$memory <- memoryDisplay(session)
    }
```
