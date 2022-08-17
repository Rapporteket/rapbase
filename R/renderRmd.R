#' Render documents from rmarkdown files at Rapporteket
#'
#' Function that renders documents at Rapporteket from rmarkdown source files.
#' Output formats may be (vanilla) HTML or PDF based on our own pandoc latex
#' template or fragments of html when the result is to be embedded in existing
#' web pages. Rmarkdown allow parameters to be part of report processing. Thus,
#' parameters that are specific to reports must be provided (as a named list)
#' when calling \code{renderRmd()}.
#'
#' @param sourceFile Character string providing the path to the rmarkdown
#' source file.
#' @param outputType Character string specifying the output format. Must be one
#' of \code{c("pdf", "html", "html_fragment")}. Default value is "html".
#' @param logoFile Character string with path to the logo to be used for PDF
#' output. Often, this will be the registry logo. Only PNG and PDF graphics are
#' allowed. Default value is \code{NULL} in which case no such logo will be
#' added to the output document.
#' @param params List of report parameters (as named values) to override the
#' corresponding entries under \emph{params} in the rmarkdown document yaml
#' header. Default is \code{NULL} in which case no parameters as defined in the
#' rmarkdown document will be overridden.
#' @param template Character string defining which template to use for making
#' pdf documents. Must be one of "default" or "document" where the first is
#' assumed if this argument is not set.
#'
#' @return Character string with path to the rendered file or, if
#' \code{outputType} is set to "html_fragment", a character string providing an
#' html fragment. Files are named according to \code{tempfile()} with an empty
#' pattern and with the extension according to \code{outputType}
#' ("pdf" or "html").
#' @export

renderRmd <- function(sourceFile, outputType = "html", logoFile = NULL,
                      params = list(), template = "default") {

  # When called from do.call (rapbase::runAutoReport()) arguments are provided
  # as class list. To prevent below switch of output formats to fail, make sure
  # outputType is of class character. See
  # https://github.com/Rapporteket/rapbase/pull/86
  outputType <- as.character(outputType)

  stopifnot(file.exists(sourceFile))
  stopifnot(outputType %in% c("html", "html_fragment", "pdf"))
  stopifnot(template %in% c("default", "document"))

  # do work in tempdir and return to origin on exit
  owd <- setwd(tempdir())
  on.exit(setwd(owd))

  # copy all files to temporary workdir
  file.copy(
    list.files(system.file("template", package = "rapbase"), full.names = TRUE),
    ".",
    overwrite = TRUE
  )
  file.copy(sourceFile, ".", overwrite = TRUE)
  if (!is.null(logoFile)) {
    file.copy(logoFile, ".", overwrite = TRUE)
  }

  f <- rmarkdown::render(
    input = basename(sourceFile),
    output_format =
      switch(outputType,
        pdf = bookdown::pdf_document2(
          pandoc_args = c(paste0("--template=", template, ".latex"))
        ),
        html = bookdown::html_document2(),
        html_fragment = bookdown::html_fragment2(),
        beamer = rmarkdown::beamer_presentation(theme = "Hannover")
      ),
    output_file = tempfile(pattern = ""),
    clean = TRUE,
    params = params,
    envir = new.env()
  )

  if (outputType == "html_fragment") {
    return(shiny::HTML(readLines(f)))
  } else {
    return(f)
  }
}
