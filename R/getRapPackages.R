#' Get all installed Rapporteket packages
#'
#' Get all installed packages that depends on 'rapbase' which itself will not
#' be reported
#'
#' @return Character vector of packages names
#' @export
#'
#' @examples
#' getRapPackages()

getRapPackages <- function() {
  allPkg <- as.data.frame(library()$result, stringsAsFactors=FALSE)
  res <- sapply(allPkg$Package, isPkgRapReg)
  res <- names(res[res==TRUE])
  res[!is.na(res)]
  
}
