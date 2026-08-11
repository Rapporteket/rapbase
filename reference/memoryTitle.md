# Title on the top left of the app, including the logo and memory usage

Title on the top left of the app, including the logo and memory usage

## Usage

``` r
memoryTitle(title)
```

## Arguments

- title:

  Title of the app

## Value

a tagList containing the logo, the title, and memory usage

## Examples

``` r
app_ui <- function() {
  shiny::navbarPage(
    title = memoryTitle("My App")
  )
}
```
