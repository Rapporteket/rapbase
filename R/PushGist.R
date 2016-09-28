#' Push some markdown to gist
#' 
#' Based on Rmd documents, push the resulting md to defined gist
#' 
#' @param mdFile String absolute file name and path of source Rmd-file. For
#' instance provided as a \code{system.file} expression. 
#' @param gistId String unike id to the gist to be updated. Defaults to an
#' empty string which means that a gist will be created. If provided the
#' corresponding gist will be updated
#' @param PAT String Personal Access Token in case the gist is not public.
#' Defaults to empty
#' @export


PushGist <- function(mdFile, gistId = "", PAT = "") {
  
  # we need some proxy...
  HTTP_PROXY <- "http://www-proxy-rn.helsenord.no:8080"
  USE_PROXY_URL <- "172.29.3.232"
  USE_PROXY_PORT <- "8080"
  Sys.setenv(http_proxy=HTTP_PROXY)
  Sys.setenv(https_proxy=HTTP_PROXY)
  
  # authenticate, if need be
  if (PAT != "") {
    tryCatch({
      Sys.setenv(GITHUB_PAT=PAT)
      gistr::gist_auth()
    }, warning = function(war) {
      return(paste("Authentication warning:", war))
    }, error = function(err) {
      return(paste("Authentication error:", err))
    })
  }
    
  if (gistId == "") {
    tryCatch({
      g <- gistr::run(mdFile, knitopts = list(quiet=TRUE))
      gistr::gist_create(g, browse=FALSE)
    }, warning = function(war) {
      return(paste("Create gist warning:", war))
    }, error = function(err){
      return(paste("Create gist error:",err))
    })
  } else {
    tryCatch({
      g <- gistr::gist(id = gistId)
      g <- gistr::update_files(g, gistr::run(mdFile, knitopts = list(quiet=TRUE)))
      g <- gistr::update(g)
    }, warning = function(war){
      return(paste("Update gist warning:",war))
    }, error = function(err) {
      return(paste("Update gist error:",err))
    })
  }
  
}