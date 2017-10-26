#' Install packages from GitHub at Rapporteket
#' 
#' Install and report results
#' 
#' @param packageName String Name of the package
#' @param branchName String Name of the branch to use
#' 
#' @return story String containing logged entries from the function
#' @export

installGithubPackage <- function(packageName, branchName) {
  
  story <- ""
  story <- MakeMessage(story, "Initiating 'InstallGithubPackage'")
  
  story <- MakeMessage(story, "Reading configuration")
  conf <- getConfig(fileName = "rapbaseConfig.yml", packageName = "rapbase")
  
  if (!exists("conf$network$proxy")) {
    story <- MakeMessage(story, "No proxy info in config. Stopping.")
    stop(story)
  }
  
  story <- MakeMessage(story, "Setting network proxies")
  Sys.setenv(http_proxy=conf$network$proxy$http)
  Sys.setenv(https_proxy=conf$network$proxy$http)
  httr::set_config(httr::use_proxy(url=conf$network$proxy$http,
                                   port=as.numeric(conf$network$proxy$port)))
  
  story <- MakeMessage(story, "Set 'libcurl' as download method")
  options(download.file.method="libcurl")
  
  githubPackage <- paste0(conf$github$organization, "/", packageName)
  githubRapbase <- paste0(conf$github$organization, "/rapbase")
  
  
  libpath <- as.character(conf$r$libpath)
  if (packageName == "rapbase") {
    story <- MakeMessage(story,
                           paste0("Intalling '", githubRapbase,
                                  "' from branch '", branchName, "'"))
    res <- tryCatch({
      devtools::install_github(githubRapbase, ref=branchName,
                               args=c("--clean",
                                      paste0("--library=", libpath)))
      
      print("'rapbase' installed")
    }, warning = function(war) {
      return(war)
    }, error = function(err) {
      return(err)
    })
    
    story <- MakeMessage(story, res)
    story <- MakeMessage(story, "Done with 'rapbase'")
  }
  
  if (packageName != "rapbase") {
    story <- MakeMessage(story, paste0("Installing '", packageName,
                                           "' from branch '", branchName, "'"))
    res <- tryCatch({
      devtools::install_github(githubPackage, ref=branchName,
                               args=c("--clean",
                                      paste0("--library=", libpath)))
      
      print(paste(packageName, "installed"))
    }, warning = function(war) {
      return(war)
    }, error = function(err) {
      return(err)
    })
    
    story <- MakeMessage(story, res)
  }
  else {
    story <- MakeMessage(story, "No additional packages to install")
  }
  
  story <- MakeMessage(story, "Done")
  
  return(story)
}