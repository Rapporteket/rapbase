#' Tools for exporting data
#'
#' Functions tools for registries that want implement exporting of registry
#' databases for local development purposes.
#'
#' @param registryName Character string registry name key
#' @param pubkey Character vector with public keys
#' @param compress Logical if export data is to be compressed (using gzip).
#' FALSE by default.
#' @param session Shiny session object
#'
#' @return Some data structure with info obtained from the github api
#' @name export
#' @aliases selectListPubkey exportDb
NULL

#' @rdname export
#' @export
selectListPubkey <- function(pubkey) {

  listName <- substr(pubkey, nchar(pubkey) - 7, nchar(pubkey))
  listName <- paste0(substr(pubkey, 1, 8), "...", listName)
  names(pubkey) <- listName

  as.list(pubkey)

}

#' @rdname export
#' @export
exportDb <- function(registryName, compress = FALSE, session) {

  stopifnot(Sys.which("mysqldump") != "")
  stopifnot(Sys.which("gzip") != "")

  f <- tempfile(pattern = registryName, fileext = ".sql")
  conf <- rapbase::getConfig()[[registryName]]
  cmd <- "mysqldump --no-tablespaces --single-transaction --add-drop-database "
  cmd <- paste0(cmd, "-B -u ", conf$user, " -p", conf$pass, " -h ", conf$host,
         " ", conf$name, " > ", f)
  invisible(system(cmd))

  if (compress) {
    inFile <- f
    f <- paste0(inFile, ".gz")
    cmd <- paste("gzip -f", inFile, ">", f)
    out <- system(cmd)
  }

  rapbase::repLogger(session, msg = paste(registryName, "db dump created."))

  invisible(f)
}
