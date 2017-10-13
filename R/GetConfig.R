#' Get configuration for package, if any
#' 
#' Try to obtain yaml-formatted configuration placed either as given by the
#' environment variable R_RAP_CONFIG_PATH or as provided by the package itself.
#' If none can be found the function exits with an error
#'
#' @param fileName String providing configuration file base name
#' @param packageName String providing the package name
#'
#' @return A list of (yaml) configuration
#' @export
#'
#' @examples
#' getConfig()

getConfig <- function(fileName = "dbConfig.yml", packageName = "rapbase") {
  
  path <- Sys.getenv("R_RAP_CONFIG_PATH")
  
  if (path == "") {
    stopifnot(file.exists(system.file(fileName, package = packageName)))
    config_file <- system.file(fileName, package = packageName)
  } else {
    stopifnot(file.exists(file.path(path, fileName)))
    config_file <- file.path(path, fileName)
  }
  
  yaml::yaml.load_file(config_file)

}