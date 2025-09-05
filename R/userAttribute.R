#' Provide user attributes based on environment context
#'
#' Extracts elements from either config, url (shiny), shiny session or
#' environmental variables relevant for user data such as name, group, role and
#' org id (\emph{e.g.} resh id). Source of info is based on environment context
#' and can be controlled by altering the default settings for which contexts
#' that will apply for the various sources of user data. This function will
#' normally be used via its helper functions (see below).
#'
#' @param entity String defining the element to return. Currently, one of
#'   'user', groups', 'resh_id', 'role', 'email', 'full_name' or 'phone'.
#'
#' @return String of single user data element
#'
#' @seealso \code{\link{getUserName}}, \code{\link{getUserGroups}},
#'   \code{\link{getUserReshId}}, \code{\link{getUserRole}}
#'
#' @export

userInfo <- function(
  entity
) {

  # check for valid entities
  if (!(entity %in% c(
    "user", "groups", "resh_id", "role", "email",
    "full_name", "phone"
  ))) {
    stop("Incorrect entity provided! Must be one of 'user', 'groups', 'resh_id'
         'role', 'email', 'full_name' or 'phone'.")
  }

  # pick the first of available user privileges
  userprivs <- as.data.frame(userAttribute(), stringsAsFactors = FALSE)[1, ]

  switch(
    entity,
    user = userprivs$name,
    groups = userprivs$group,
    resh_id = userprivs$org,
    role = userprivs$role,
    email = userprivs$email,
    full_name = userprivs$fullName,
    phone = userprivs$phone
  )
}


#' User attributes in container apps running behind shinyproxy
#'
#' For apps running as containers particular environment variables must be
#' defined for an orderly handling of dynamic user privileges. This function
#' makes use of environmental variables defined by shinyproxy to provide
#' available privileges for the shiny application.
#'
#' @param unit Integer providing the look-up unit id. Default value is NULL in
#'   which case all privileges for \code{group} are returned.
#' @param map_orgname A data.frame containing two columns:
#'   \describe{
#'    \item{UnitId}{unit ids}
#'    \item{orgname}{corresponding organization names}
#'    }
#'
#' @return Invisibly a list of user metadata and privileges:
#'   \describe{
#'     \item{name}{The username for whom the privileges apply.}
#'     \item{fullName}{User full name}
#'     \item{phone}{User phone number}
#'     \item{email}{User email}
#'     \item{unit}{Unit id under which the privileges are defined.}
#'     \item{org}{Organization id for the user.}
#'     \item{role}{Role of the user.}
#'     \item{orgName}{Name of the organization as defined under the unit id.}
#'   }
#' @export

userAttribute <- function(unit = NULL,
                          map_orgname = NULL) {

  if (Sys.getenv("FALK_EXTENDED_USER_RIGHTS") == "" ||
        Sys.getenv("FALK_APP_ID") == "") {
    stop(paste(
      "Environmental variables",
      "FALK_EXTENDED_USER_RIGHTS and FALK_APP_ID",
      "must both be set!"
    ))
  }

  tilganger <- jsonlite::parse_json(
    Sys.getenv("FALK_EXTENDED_USER_RIGHTS"),
    simplifyVector = TRUE
  )
  tilganger <- tilganger[tilganger$A == Sys.getenv("FALK_APP_ID"), ]

  # restrict when unit is provided
  if (!is.null(unit)) {
    tilganger <- tilganger[tilganger$U == unit, ]
  }

  groups <- tilganger$A
  units <- tilganger$U

  orgs <- tilganger$U
  roles <- tilganger$R

  if (!is.null(map_orgname)) {
    orgNames <- map_orgname$orgname[match(orgs, map_orgname$UnitId)]
  } else {
    orgNames <- rep("Ukjent", length(units))
  }

  list(
    name = Sys.getenv("SHINYPROXY_USERNAME"),
    fullName = Sys.getenv("FALK_USER_FULLNAME"),
    phone = Sys.getenv("FALK_USER_PHONE"),
    email = Sys.getenv("FALK_USER_EMAIL"),
    group = groups,
    unit = units,
    org = orgs,
    role = roles,
    orgName = orgNames
  )
}


#' Get user attributes
#'
#' These are helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to these functions is made without any arguments.
#' If redefining contexts is needed, please use \code{\link{userInfo}} instead.
#'
#' @param ... To keep the same interface as before. Not used.
#' @return String with user attribute
#' @name userAttribute
#' @aliases getUserEmail getUserFullName getUserGroups getUserName getUserPhone
#'   getUserReshId getUserRole
#'
#' @examples
#' \donttest{
#' try(getUserEmail())
#' }
NULL


#' @rdname userAttribute
#' @export
getUserEmail <- function(...) {
  userInfo(entity = "email")
}


#' @rdname userAttribute
#' @export
getUserFullName <- function(...) {
  userInfo(entity = "full_name")
}


#' @rdname userAttribute
#' @export
getUserGroups <- function(...) {
  userInfo(entity = "groups")
}


#' @rdname userAttribute
#' @export
getUserName <- function(...) {
  userInfo(entity = "user")
}


#' @rdname userAttribute
#' @export
getUserPhone <- function(...) {
  userInfo(entity = "phone")
}


#' @rdname userAttribute
#' @export
getUserReshId <- function(...) {
  userInfo(entity = "resh_id")
}


#' @rdname userAttribute
#' @export
getUserRole <- function(...) {
  userInfo(entity = "role")
}
