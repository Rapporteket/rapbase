#' Rapporteket context
#'
#' Call to this function will return TRUE when run on a system where the
#' environment variable \code{R_RAP_INSTANCE} is set to either "DEV", "TEST",
#' "QA" or "PRODUCTION" and FALSE otherwise
#'
#' @return Logical if system has a defined Rapporteket context
#' @export
#'
#' @examples
#' isRapContext()
#'
isRapContext <- function() {
  if (Sys.getenv("R_RAP_INSTANCE") %in%
        c("DEV", "TEST", "QA", "QAC", "PRODUCTION", "PRODUCTIONC")) {
    TRUE
  } else {
    FALSE
  }
}


#' Make standard table for rmarkdown reports
#'
#' Function that will return tables used in reports.
#'
#' \code{mst()} creates RMarkdown code for creating standard tables.
#'
#' @param tab Data frame or matrix representing the table.
#' @param col_names Character vector with column names. Defaults
#' \code{colnames(tab)}.
#' @param type Character string defining output, either "html" or "pdf".
#' Default is "pdf".
#' @param cap Character string with table caption. Empty string by default.
#' @param label Character string defining the label in case the table needs to
#' be referenced elsewhere in the overall document. For instance, setting this
#' to 'my_table' the corresponding inline rmarkdown reference to use is
#' \code{\\@ref(tab:my_table)}. Please note that for this to work for both
#' LaTex and HTML the bookdown document processing functions must be used,
#' \emph{i.e.} \code{bookdown::pdf_document2()} and
#' \code{bookdown::html_document2()}, respectively. Default value is
#' \code{knitr::opts_current$get("label")} in which case the name of the
#' current R chunk will be used as label.
#' @param digs Integer number of digits to use. 0 by default.
#' @param align Character vector specifying column alignment in the LaTeX way,
#' \emph{e.g.} \code{c("l", "c", "r")} will align the first column to the left,
#' center the second and right-align the last one. Default is NULL in which
#' case numeric columns are right-aligned and all other columns are
#' left-aligned.
#' @param fs Integer providing the font size. Applies only for pdf output.
#' Default value is 8.
#' @param lsd Logical if table is to be scaled down. Applies only for pdf
#' output. FALSE by default.
#'
#' @return Character string containing RMarkdown table code
#' @name makeStandardTable
#' @aliases mst
#' @examples
#' mst(tab = mtcars[1:10, ])
NULL


#' @rdname makeStandardTable
#' @export
mst <- function(tab, col_names = colnames(tab), type = "pdf", cap = "",
                label = knitr::opts_current$get("label"), digs = 0,
                align = NULL, fs = 8, lsd = FALSE) {
  if (type == "pdf") {
    if (lsd) {
      lo <- c("HOLD_position", "scale_down")
    } else {
      lo <- c("HOLD_position")
    }
    k <- knitr::kable(tab,
      format = "latex", col.names = col_names, caption = cap,
      label = label, digits = digs,
      align = align, booktabs = TRUE
    ) %>%
      kableExtra::kable_styling(latex_options = lo, font_size = fs)
  }

  if (type == "html") {
    k <- knitr::kable(tab,
      format = "html", col.names = col_names, caption = cap,
      label = label, digits = digs,
      align = align
    ) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE
      )
  }
  k
}


#' Provide a no-opt-out ok message
#'
#' To be applied for user input when there is actually no choice :-)
#'
#' @return String with possible state of mind (in Norwegian) once left with no
#' options
#' @export
#'
#' @examples
#' noOptOutOk()
noOptOutOk <- function() {
  msg <- c(
    "Den er grei", "Om du sier det, s\u00e5", "Ok, da", "Javel, da",
    "Ja, da er det vel s\u00e5nn", "Sk\u00e5l for den!", "Fint", "Flott",
    "Klart", "Mottatt", "Akkurat", "Livet er herlig"
  )

  sample(msg, 1)
}

#' Plain testing tool
#'
#' To be used for testing purposes
#'
#' @return message A test message
#' @export

halloRapporteket <- function() { # nocov start

  testMessage <- "Hallaisen Rapporteket"
  message(testMessage)
  # nocov end
}
