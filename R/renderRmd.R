#' Render documents from rmarkdown files at Rapporteket
#'
#' Function that renders documents at Rapporteket from rmarkdown sorce files.
#' Output formats may be (vanilla) HTML or PDF based on our own pandoc latex
#' template. Allow for parameters to be part of report processing. However,
#' parameters that are speciffic to reports must be provided (as a named list)
#' when calling this function.
#'
#' @param sourceFile Character string providing the path to the rmarkdown
#' source file.
#' @param outputType Character string giving the output format. Must be either
#' "pdf" or "html" (default).
#' @param logoFile Character string with path to the logo to be used.
#' Often, this will be the registry logo. Only png and pdf graphics are
#' allowed. Default value is \code{NULL} in which case no such logo will be
#' added to the output document.
#' @param params List of report parameters (as named values) to override the
#' corresponding entries under \emph{params} in the rmarkdown document yaml
#' header. Default is \code{NULL} in which case no parameters as defined in the
#' rmarkdown document will be overridden.
#'
#' @return Character string with path to the rendered file. The file is named
#' according to \code{tempfile()} with an empty pattern and with the
#' extention according to \code{outputType} ("pdf" or "html")
#' @export

renderRmd <- function(sourceFile, outputType = "html", logoFile = NULL,
											params = list()) {

	stopifnot(file.exists(sourceFile))
	stopifnot(outputType %in% c("html", "pdf"))

	# do work in tempdir and return to origin on exit
	owd <- setwd(tempdir())
	on.exit(setwd(owd))

	# copy all files to temporary workdir
	templateFiles <- c("default.latex", "logo.png")
	file.copy(system.file(
		file.path("template", templateFiles), package = "rapbase"), ".",
						overwrite = TRUE)
	file.copy(sourceFile, ".", overwrite = TRUE)
	if (!is.null(logoFile)) {
		print(logoFile)
		file.copy(logoFile, ".", overwrite = TRUE)
	}

	print(sourceFile)

	rmarkdown::render(
		input = basename(sourceFile),
		output_format =
			switch(
				outputType,
				pdf = bookdown::pdf_document2(pandoc_args = c("--template=default.latex")),
				html = bookdown::html_document2(),
				beamer = rmarkdown::beamer_presentation(theme = "Hannover"),
				reveal = revealjs::revealjs_presentation(theme = "sky")
			),
		#intermediates_dir = tempdir(),
		output_file = tempfile(pattern = ""),
		clean = TRUE,
		params = params,
		envir = new.env())

}