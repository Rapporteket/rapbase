#' RunNoweb
#'
#' Function to run noweb file contained in a package. Assume all noweb files
#' of the package are placed flat under the \emph{inst} directory
#'
#' @param nowebFileName Basename of the noweb file, \emph{e.g.} 'myFile.Rnw'.
#' @param packageName Name of the package containing noweb file(s)
#' @param weaveMethod Method to apply for weaving. Currently available are
#'  'Sweave' and 'knitr', default to the latter.
#' @importFrom utils Sweave
#' @export

RunNoweb <- function(nowebFileName, packageName, weaveMethod = "knitr") {
  weaveInputFile <- system.file(nowebFileName, package = packageName)
  if (weaveMethod == "knitr") {
    # make sure processing takes place "here" (sessions getwd())
    knitr::opts_knit$set(root.dir='./')
    # make sure we do not make figure folder
    knitr::opts_chunk$set(fig.path='')
    knitr::knit(weaveInputFile)
  } else if (is.element(weaveMethod, c("Sweave", "sweave"))) {
    Sweave(weaveInputFile, encoding="utf8")
  } else {
    cat("\nweaveMethod specified is none of knitr or Sweave. Nothing to do\n")
  }
}
