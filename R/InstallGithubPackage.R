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
  
  message <- MakeMessage(message, "Reading configuration")
  conf <- yaml::yaml.load_file(system.file("rapbaseConfig.yml",
                                           package = "rapbase"))
  
  pConf <- conf$network$proxy
  HTTP_PROXY <- pConf$http
  PROXY_IP <- pConf$ip
  PROXY_PORT <- pConf$port
  
  message <- MakeMessage(message, "Setting network proxies")
  Sys.setenv(http_proxy=HTTP_PROXY)
  Sys.setenv(https_proxy=HTTP_PROXY)
  httr::set_config(httr::use_proxy(url=PROXY_IP,
                                   port=as.numeric(PROXY_PORT)))
  
  message <- MakeMessage(message, "Set 'libcurl' as download method")
  options(download.file.method="libcurl")
  
  GITHUB_ORGANIZATION <- conf$github$organization
  githubPackage <- paste0(GITHUB_ORGANIZATION, "/", packageName)
  githubRapbase <- paste0(GITHUB_ORGANIZATION, "/rapbase")
  
  
  libpath <- as.character(conf$r$libpath)
  if (packageName == "rapbase") {
    message <- MakeMessage(message,
                           paste0("Intalling '", githubRapbase,
                                  "' from branch '", branchName, "'"))
    res <- tryCatch({
      withr::with_libpaths(new = libpath,
                           devtools::install_github(githubRapbase,
                                                    ref=branchName,
                                                    force=TRUE,
                                                    args=c("--clean")))
      print("'rapbase' installed")
    }, warning = function(war) {
      return(war)
    }, error = function(err) {
      return(err)
    })
    
    message <- MakeMessage(message, res)
    message <- MakeMessage(message, "Done with 'rapbase'")
  }
  
  if (packageName != "rapbase") {
    message <- MakeMessage(message, paste0("Installing '", packageName,
                                           "' from branch '", branchName, "'"))
    res <- tryCatch({
      withr::with_libpaths(new = libpath,
                              devtools::install_github(githubPackage,
                                                       ref=branchName,
                                                       args=c("--clean")))
      
      print(paste(packageName, "installed"))
    }, warning = function(war) {
      return(war)
    }, error = function(err) {
      return(err)
    })
    
    message <- MakeMessage(message, res)
  }
  else {
    message <- MakeMessage(message, "No additional packages to install")
  }
  
  message <- MakeMessage(message, "Done")
  
  return(message)
}