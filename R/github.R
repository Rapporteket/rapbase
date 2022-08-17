#' Collect various data from the GitHub API
#'
#' @param what Character string defining the api endpoint. Currently one of
#' \code{c("contributors", "members", "keys")}.
#' @param value Character string specifying what to collect from the given
#' endpoint. For "contributors" this should be the name of the repository, for
#' "members" value should be the team slug and for "keys" this should be a
#' github user name.
#' @param .token Character string providing av valid token that will be used if
#' the api call requires authentication. Listing of team members do require a
#' token with the appropriate credentials.
#'
#' @return Character vector with results from the GitHub api request
#' @export

getGithub <- function(what, value, .token = NULL) {
  stopifnot(what %in% c("contributors", "members", "keys"))

  conf <- rapbase::getConfig("rapbaseConfig.yml")

  if (what %in% c("contributors")) {
    endpoint <- paste0("/repos/rapporteket/", value, "/contributors")
    vName <- "login"
  }

  # nocov start
  if (what %in% c("members")) {
    endpoint <- paste0("/orgs/rapporteket/teams/", value, "/members")
    vName <- "login"
  }
  # nocov end

  if (what %in% c("keys")) {
    endpoint <- paste0("/users/", value, "/keys")
    vName <- "key"
  }

  sship::gh(
    path = endpoint, proxy_url = conf$network$proxy$http, token = .token
  )$content[[vName]]
}
