#' Install packages from GitHub at Rapporteket
#' 
#' Install and report results
#' 
#' @param packageName String Name of the package
#' @param branchName String Name of the branch to use
#' 
#' @return message String containing logged entries from the function
#' @export

installGithubPackage <- function(packageName, branchName) {
  
  message <- ""
  message <- MakeMessage(message, "Initiating 'InstallGithubPackage'")
  
  HTTP_PROXY <- "http://www-proxy.helsenord.no:8080"
  USE_PROXY_URL <- "172.29.3.227"
  USE_PROXY_PORT <- "8080"
  GITHUB_ORGANIZATION <- "Rapporteket"
  
  githubPackage <- paste0(GITHUB_ORGANIZATION, "/", packageName)
  githubRapbase <- paste0(GITHUB_ORGANIZATION, "/rapbase")
  
  message <- MakeMessage(message, "Setting network proxies")
  Sys.setenv(http_proxy=HTTP_PROXY)
  httr::set_config(httr::use_proxy(url=USE_PROXY_URL,
                                   port=as.numeric(USE_PROXY_PORT)))

  message <- MakeMessage(message,
                         paste0("Intalling 'rapbase' from branch '",
                                branchName, "'"))
  devtools::with_libpaths(new = "/usr/lib/R/site-library",
                          install_github(githubRapbase, ref=branchName,
                                         args=c("--clean")))
  message <- MakeMessage(message, "Done with 'rapbase'")
  
  if (packageName != "rapbase") {
    message <- MakeMessage(message, paste0("Installing '", packageName,
                                           "' from branch '", branchName, "'"))
    devtools::install_github(githubPackage, ref=branchName)
    
  }
  else {
    message <- MakeMessage(message, "No additional packages to install")
  }
  
  message <- MakeMessage(message, "Done")
  
  return(message)
}