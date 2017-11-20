#' Push some markdown to gist
#' 
#' Based on Rmd documents, push the resulting md to defined gist
#' 
#' @param mdFile String absolute file name and path of source Rmd-file. For
#' instance provided as a \code{system.file} expression. 
#' @param githubUserName String github user name for whom the gist will be
#' published 
#' @export


PushGist <- function(mdFile, githubUserName = "") {
  
  reportName <- strsplit(basename(mdFile), ".", fixed = TRUE)[[1]][1]
  
  conf <- getConfig()
  
  # we need some proxy...
  pConf <- conf$network$proxy
  HTTP_PROXY <- pConf$http
  USE_PROXY_URL <- pConf$ip
  USE_PROXY_PORT <- pConf$port
  Sys.setenv(http_proxy=HTTP_PROXY)
  Sys.setenv(https_proxy=HTTP_PROXY)
  
  # authenticate, if need be
  PAT <- conf$github$PAT[githubUserName]
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
  
  gistId <- as.character(conf$github$gistId[reportName])
  if (gistId == "") {
    tryCatch({
      g <- gistr::run(mdFile, knitopts = list(quiet=TRUE))
      gistr::gist_create(g, browse=FALSE)
    }, warning = function(war) {
      return(paste("Create gist warning:", war))
    }, error = function(err){
      return(paste("Create gist error:", err))
    })
  } else {
    tryCatch({
      g <- gistr::gist(id = gistId)
      g <- gistr::update_files(g, gistr::run(mdFile, knitopts = list(quiet=TRUE)))
      g <- gistr::update(g)
    }, warning = function(war){
      return(paste("Update gist warning:", war))
    }, error = function(err) {
      return(paste("Update gist error:", err))
    })
  }
  
}