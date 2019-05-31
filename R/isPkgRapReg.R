#' Test if a package is part of Rapporteket
#'
#' Test if an installed package is linked to Rapportekt based on
#' dependency to the package 'rapbase'
#'
#' @param pkg String providing the package name
#'
#' @return Logical TRUE if pkg depends on 'rapbase', FALSE if not
#' @export
#'
#' @seealso \code{\link{getRapPackages}} on how to list all packages that
#' depend om 'rapbase'
#'
#' @examples
#' # returns FALSE, rapbase has no explicit dependency to itself
#' isPkgRapReg("rapbase")


isPkgRapReg <- function(pkg) {
  grepl("rapbase", utils::packageDescription(pkg)$Depends, fixed = TRUE)
}