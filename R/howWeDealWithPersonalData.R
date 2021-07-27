#' Render text in pop-up
#'
#' Render text on how Rapporteket deals witn personal data
#'
#' @param session A shiny session object used to personalize the text
#' @param callerPkg Character string naming the package that makes a call to
#' this function in case version number of the caller package should be added
#' to the returned (html) info text. Default to NULL in which case no version
#' number for the caller will be added to the info text
#'
#' @return fragment html info text
#' @export

howWeDealWithPersonalData <- function(session, callerPkg = NULL) {

  pkg <- list()
  pkg$name <- as.vector(utils::installed.packages()[, 1])
  pkg$ver <- as.vector(utils::installed.packages()[, 3])

  pkgs <- intersect(c("shiny", "rapbase"), pkg$name)

  if (!is.null(callerPkg)) {
    if (callerPkg %in% pkg$name) {
      pkgs <- c(pkgs, callerPkg)
    } else {
      warning(paste(callerPkg, "is not an installed package."))
    }
  }

  ind <- pkg$name %in% pkgs
  pkgs <- pkg$name[ind]
  vers <- pkg$ver[ind]

  # add R itself
  pkgs <- c("R", pkgs)
  vers <- c(paste(R.version$major, R.version$minor, sep = "."), vers)

  pkgInfo <- paste0(pkgs, vers, collapse = ", ")

  sourceFile <- system.file(
    "howWeDealWithPersonalData.Rmd", package = "rapbase")

  rapbase::renderRmd(sourceFile = sourceFile, outputType = "html_fragment",
                     params = list(session = session,
                                   pkgInfo = pkgInfo))
}
