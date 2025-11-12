# Render documents from rmarkdown files at Rapporteket

Function that renders documents at Rapporteket from rmarkdown source
files. Output formats may be (vanilla) HTML or PDF based on our own
pandoc latex template or fragments of html when the result is to be
embedded in existing web pages. Rmarkdown allow parameters to be part of
report processing. Thus, parameters that are specific to reports must be
provided (as a named list) when calling `renderRmd()`.

## Usage

``` r
renderRmd(
  sourceFile,
  outputType = "html",
  logoFile = NULL,
  params = list(),
  template = "default"
)
```

## Arguments

- sourceFile:

  Character string providing the path to the rmarkdown source file.

- outputType:

  Character string specifying the output format. Must be one of
  `c("pdf", "html", "html_fragment")`. Default value is "html".

- logoFile:

  Character string with path to the logo to be used for PDF output.
  Often, this will be the registry logo. Only PNG and PDF graphics are
  allowed. Default value is `NULL` in which case no such logo will be
  added to the output document.

- params:

  List of report parameters (as named values) to override the
  corresponding entries under *params* in the rmarkdown document yaml
  header. Default is `NULL` in which case no parameters as defined in
  the rmarkdown document will be overridden.

- template:

  Character string defining which template to use for making pdf
  documents. Must be one of "default" or "document" where the first is
  assumed if this argument is not set.

## Value

Character string with path to the rendered file or, if `outputType` is
set to "html_fragment", a character string providing an html fragment.
Files are named according to
[`tempfile()`](https://rdrr.io/r/base/tempfile.html) with an empty
pattern and with the extension according to `outputType` ("pdf" or
"html").
