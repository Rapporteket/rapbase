#' Collect various data from the GitHub API
#'
#' @param what Character string defining the api endpoint. Currently one of
#' \code{c("contributors", "keys")}.
#' @param value Character string specifying what to collect from the given
#' endpoint. For "contributors" this should be the name of the repository and
#' for "keys" this should be a github user name.
#' @param .token Character string providing av valid token that will be used if
#' the api call requires authentication.
#'
#' @return Character vector with results from the GitHub api request
#' @export

getGithub <- function(what, value, .token = NULL) {

  stopifnot(what %in% c("contributors", "keys"))

  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml",
                             packageName = "rapbase")

  if (what %in% c("contributors")) {
    endpoint <- paste0("/repos/rapporteket/", value, "/contributors")
    vName <- "login"
  }

  if (what %in% c("keys")) {
    endpoint <- paste0("/users/", value, "/keys")
    vName <- "key"
  }

  httr::set_config(httr::use_proxy(url = conf$network$proxy$http))

  vapply(
    gh::gh(endpoint, .token = .token),
    "[[", "", vName
  )
}
