#' Install packages from GitHub at Rapporteket
#'
#' Install and report results
#'
#' @param packageName String Name of the package
#' @param branchName String Name of the branch to use
#' @param readConfig Set to FALSE to prevent function from reading
#' configuration. Set TRUE by default. Mainly used for testing purposes.
#'
#' @return story String containing logged entries from the function
#' @export

installGithubPackage <- function(packageName, branchName = "master",
                                 readConfig=TRUE) {

  # nocov start

  story <- ""
  story <- makeMessage(story, "Initiating 'InstallGithubPackage'")

  story <- makeMessage(story, "Reading configuration")

  if (readConfig) {
    conf <- getConfig(fileName = "rapbaseConfig.yml", packageName = "rapbase")
  } else {
    conf <- list(network = list(notAProxy = "test"))
  }

  if (is.null(conf$network[["proxy"]])) {
    story <- makeMessage(story, "Proxy not defined in config. If your system
                         does not use one please provide it as an empty string.
                         Stopping.")
    stop(story)
  }

  story <- makeMessage(story, "Setting network proxies")
  if (!is.null(conf$network$proxy$http)) {
    Sys.setenv(http_proxy = conf$network$proxy$http)
    Sys.setenv(https_proxy = conf$network$proxy$http)
    httr::set_config(httr::use_proxy(url = conf$network$proxy$http,
      port = as.numeric(conf$network$proxy$port)))
  }
  story <- makeMessage(story, "Set 'libcurl' as download method")
  options(download.file.method = "libcurl")

  githubPackage <- paste0(conf$github$organization, "/", packageName)
  githubRapbase <- paste0(conf$github$organization, "/rapbase")

  success <- paste0("'", packageName, "' installed")
  if (packageName == "rapbase") {
    story <- makeMessage(story,
                           paste0("Intalling '", githubRapbase,
                                  "' from branch '", branchName, "'"))
    res <- tryCatch({
      remotes::install_github(githubRapbase, ref = branchName)

      success
    }, warning = function(war) {
      return(war) # nocov
    }, error = function(err) {
      return(err)
    })

    story <- makeMessage(story, res)
    story <- makeMessage(story, "Done with 'rapbase'")
  }

  if (packageName != "rapbase") {
    story <- makeMessage(story, paste0("Installing '", packageName,
                                       "' from branch '", branchName, "'"))
    res <- tryCatch({
      remotes::install_github(githubPackage, ref = branchName)

      success
    }, warning = function(war) {
      return(war) #nocov
    }, error = function(err) {
      return(err)
    })

    story <- makeMessage(story, res)
  }
  else {
    story <- makeMessage(story, "No additional packages to install")
  }

  story <- makeMessage(story, "Done")

  return(story)

  # nocov end
}