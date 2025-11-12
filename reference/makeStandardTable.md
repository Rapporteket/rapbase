# Make standard table for rmarkdown reports

Function that will return tables used in reports.

## Usage

``` r
mst(
  tab,
  col_names = colnames(tab),
  type = "pdf",
  cap = "",
  label = knitr::opts_current$get("label"),
  digs = 0,
  align = NULL,
  fs = 8,
  lsd = FALSE
)
```

## Arguments

- tab:

  Data frame or matrix representing the table.

- col_names:

  Character vector with column names. Defaults `colnames(tab)`.

- type:

  Character string defining output, either "html" or "pdf". Default is
  "pdf".

- cap:

  Character string with table caption. Empty string by default.

- label:

  Character string defining the label in case the table needs to be
  referenced elsewhere in the overall document. For instance, setting
  this to 'my_table' the corresponding inline rmarkdown reference to use
  is `\@ref(tab:my_table)`. Please note that for this to work for both
  LaTex and HTML the bookdown document processing functions must be
  used, *i.e.*
  [`bookdown::pdf_document2()`](https://pkgs.rstudio.com/bookdown/reference/html_document2.html)
  and
  [`bookdown::html_document2()`](https://pkgs.rstudio.com/bookdown/reference/html_document2.html),
  respectively. Default value is `knitr::opts_current$get("label")` in
  which case the name of the current R chunk will be used as label.

- digs:

  Integer number of digits to use. 0 by default.

- align:

  Character vector specifying column alignment in the LaTeX way, *e.g.*
  `c("l", "c", "r")` will align the first column to the left, center the
  second and right-align the last one. Default is NULL in which case
  numeric columns are right-aligned and all other columns are
  left-aligned.

- fs:

  Integer providing the font size. Applies only for pdf output. Default
  value is 8.

- lsd:

  Logical if table is to be scaled down. Applies only for pdf output.
  FALSE by default.

## Value

Character string containing RMarkdown table code

## Details

`mst()` creates RMarkdown code for creating standard tables.

## Examples

``` r
mst(tab = mtcars[1:10, ])
#> \begin{table}[H]
#> \centering
#> \caption{}
#> \centering
#> \fontsize{8}{10}\selectfont
#> \begin{tabular}[t]{lrrrrrrrrrrr}
#> \toprule
#>   & mpg & cyl & disp & hp & drat & wt & qsec & vs & am & gear & carb\\
#> \midrule
#> Mazda RX4 & 21 & 6 & 160 & 110 & 4 & 3 & 16 & 0 & 1 & 4 & 4\\
#> Mazda RX4 Wag & 21 & 6 & 160 & 110 & 4 & 3 & 17 & 0 & 1 & 4 & 4\\
#> Datsun 710 & 23 & 4 & 108 & 93 & 4 & 2 & 19 & 1 & 1 & 4 & 1\\
#> Hornet 4 Drive & 21 & 6 & 258 & 110 & 3 & 3 & 19 & 1 & 0 & 3 & 1\\
#> Hornet Sportabout & 19 & 8 & 360 & 175 & 3 & 3 & 17 & 0 & 0 & 3 & 2\\
#> \addlinespace
#> Valiant & 18 & 6 & 225 & 105 & 3 & 3 & 20 & 1 & 0 & 3 & 1\\
#> Duster 360 & 14 & 8 & 360 & 245 & 3 & 4 & 16 & 0 & 0 & 3 & 4\\
#> Merc 240D & 24 & 4 & 147 & 62 & 4 & 3 & 20 & 1 & 0 & 4 & 2\\
#> Merc 230 & 23 & 4 & 141 & 95 & 4 & 3 & 23 & 1 & 0 & 4 & 2\\
#> Merc 280 & 19 & 6 & 168 & 123 & 4 & 3 & 18 & 1 & 0 & 4 & 4\\
#> \bottomrule
#> \end{tabular}
#> \end{table}
```
