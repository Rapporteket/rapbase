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
#' @param shinySession Shiny session object (list, NULL by default). Must be
#'   provided when the source of user attributes is either the shiny app url or
#'   an external authentication provider. By default this will apply to the
#'   'TEST', 'QA' and 'PRODUCTION' contexts in which case the shiny session
#'   object must be provided.
#' @param devContexts A character vector providing unique instances to be
#'   regarded as a development context. In this context user attributes will be
#'   read from configuration as provided by 'rapbaseConfig.yml'. The instances
#'   provided cannot overlap instances provided in any other contexts. By
#'   default set to \code{c("DEV")}.
#' @param testContexts A character vector providing unique instances to be
#'   regarded as a test context. In this context user attributes will be read
#'   from the url call to a shiny application. Hence, for this context the
#'   corresponding shiny session object must also be provided. The instances
#'   provided cannot overlap instances provided in any other contexts. By
#'   default set to \code{c("TEST")}.
#' @param prodContexts A character vector providing unique instances to be
#'   regarded as a production context. In this context user attributes will be
#'   read from the shiny session object (on deployment in shiny-server) or, from
#'   environmental variables (on standalone container deployment). Hence, for
#'   this context the corresponding shiny session object must also be provided.
#'   Instances provided cannot overlap instances in any other contexts. By
#'   default set to \code{c("QA", "QAC", "PRODUCTION", "PRODUCTIONC")}.
#'   Duplication as seen by the "C" suffix will be needed as long as apps in
#'   question are to be run on both shiny-server and as standalone containers.
#' @param group Character string providing the name of the app R package name.
#'   The term "group" is used to relate to the environmental variable
#'   SHINYPROXY_USERGROUPS that corresponds to the apps a given user can access.
#'
#' @return String of single user data element
#'
#' @seealso \code{\link{getUserName}}, \code{\link{getUserGroups}},
#'   \code{\link{getUserReshId}}, \code{\link{getUserRole}}
#'
#' @export

userInfo <- function(
    entity,
    shinySession = NULL,
    devContexts = c("DEV"),
    testContexts = c("TEST"),
    prodContexts = c("QA", "QAC", "PRODUCTION", "PRODUCTIONC"),
    group = NULL
) {

  # stop helper function
  stopifnotShinySession <- function(object) {
    if (!inherits(
      shinySession, c("ShinySession", "session_proxy", "MockShinySession")
    )) {
      stop(paste(
        "'ShinySession' argument is not a shiny session object! Cannot go on."
      ))
    } else {
      invisible()
    }
  }

  # check for valid entities
  if (!(entity %in% c(
    "user", "groups", "resh_id", "role", "email",
    "full_name", "phone"
  ))) {
    stop("Incorrect entity provided! Must be one of 'user', 'groups', 'resh_id'
         'role', 'email', 'full_name' or 'phone'.")
  }

  # check if any contexts overlap, and stop if so
  if (any(table(c(devContexts, testContexts, prodContexts)) > 1)) {
    stop("Contexts overlapping! Please adjust. Stopping.")
  }

  # get current system context
  context <- Sys.getenv("R_RAP_INSTANCE")

  if (context == "") {
    message("System has no defined instance. Configuration as provided by
            'rapbaseConfig.yml' will be used as source for user data.")
    conf <- getConfig(fileName = "rapbaseConfig.yml")
    d <- conf$r$testUser
    user <- d$user
    groups <- d$groups
    role <- d$role
    resh_id <- d$resh_id
    email <- d$email
    full_name <- d$full_name
    phone <- d$phone
  }

  if (context %in% devContexts) {

    stopifnotShinySession(shinySession)

    conf <- getConfig(fileName = "rapbaseConfig.yml")
    d <- conf$r$testUser
    user <- d$user
    groups <- d$groups
    role <- d$role
    resh_id <- d$resh_id
    email <- d$email
    full_name <- d$full_name
    phone <- d$phone
  }

  if (context %in% testContexts) {

    stopifnotShinySession(shinySession)

    us <- shiny::parseQueryString(shinySession$clientData$url_search)
    user <- us$`X-USER`
    groups <- us$`X-GROUPS`
    resh_id <- us$resh_id
    role <- us$role
    email <- us$email
    full_name <- us$full_name
    phone <- us$phone
  }

  if (context %in% prodContexts) {

    stopifnotShinySession(shinySession)

    if (context %in% c("QA", "PRODUCTION")) {
      user <- shinySession$user
      groups <- shinySession$groups
      resh_id <- shinySession$request$HTTP_RESHID
      role <- shinySession$request$HTTP_ROLE
      email <- shinySession$request$HTTP_EMAIL
      full_name <-
        parse(text = paste0("'", shinySession$request$HTTP_FULLNAME, "'"))[[1]]
      phone <- shinySession$request$HTTP_PHONE
    }

    if (context %in% c("QAC", "PRODUCTIONC")) {
      userprivs <- userAttribute(group)
      # pick the first of available user privileges
      userprivs <- as.data.frame(userprivs, stringsAsFactors = FALSE)[1, ]
      user <- userprivs$name
      groups <- userprivs$group
      resh_id <- userprivs$org
      role <- userprivs$role
      email <- userprivs$email
      full_name <- userprivs$fullName
      phone <- userprivs$phone
    }
  }

  switch(entity,
    user = user,
    groups = groups,
    resh_id = resh_id,
    role = role,
    email = email,
    full_name = full_name,
    phone = phone
  )
}


#' User attributes in container apps running behind shinyproxy
#'
#' For apps running as containers particular environment variables must be
#' defined for an orderly handling of dynamic user privileges. This function
#' makes use of environmental variables defined by shinyproxy to provide
#' available privileges for the shiny application.
#'
#' @param group Character string providing the name of the app R package name.
#'   The term "group" is used to relate to the environmental variable
#'   SHINYPROXY_USERGROUPS that corresponds to the apps a given user can access.
#' @param unit Integer providing the look-up unit id. Default value is NULL in
#'   which case all privileges for \code{group} are returned.
#'
#' @return Invisibly a list of user metadata and privileges:
#'   \describe{
#'     \item{name}{The username for whom the privileges apply.}
#'     \item{fullName}{User full name}
#'     \item{phone}{User phone number}
#'     \item{email}{User email}
#'     \item{group}{Group of which the user is a member.}
#'     \item{unit}{Unit id under which the privileges are defined.}
#'     \item{org}{Organization id for the user.}
#'     \item{role}{Role of the user.}
#'     \item{orgName}{Name of the organization as defined under the unit id.}
#'   }
#' @export

userAttribute <- function(group, unit = NULL) {

  stopifnot(group %in% utils::installed.packages()[, 1])

  if (Sys.getenv("SHINYPROXY_USERGROUPS") == "" ||
      Sys.getenv("USERORGID") == "") {
    stop(paste(
      "Environmental variables SHINYPROXY_USERGROUPS and USERORGID must both",
      "be set!"
    ))
  }

  name <- Sys.getenv("SHINYPROXY_USERNAME")
  fullName <- parse(text = paste0("'", Sys.getenv("USERFULLNAME"), "'"))[[1]]
  phone <- Sys.getenv("USERPHONE")
  email <- Sys.getenv("USEREMAIL")

  # make vectors of vals
  units <- unlist(
    strsplit(
      gsub("\\s|\\[|\\]", "", Sys.getenv("USERORGID")),
      ","
    )
  )

  groups <- unlist(
    strsplit(
      gsub("\\s|\\[|\\]", "", Sys.getenv("SHINYPROXY_USERGROUPS")),
      ","
    )
  )

  if (length(units) != length(groups)) {
    stop(paste(
      "Vectors obtained from SHINYPROXY_USERGROUPS and USERORGID are of",
      "different lengths. Hence, correspondence cannot be anticipated."
    ))
  }

  # NB Anticipate that element positions in vectors do correspond!
  ## filter by this group
  units <- units[groups == group]
  groups <- groups[groups == group]

  ## restrict when unit is provided
  if (!is.null(unit)) {
    groups <- groups[units == unit]
    units <- units[units == unit]
  }

  # Look up org, role and unit name
  orgs <- vector()
  roles <- vector()
  orgNames <- vector()
    for (i in seq_len(length(units))) {
    orgs[i] <- unitAttribute(units[i], "resh")
    roles[i] <- unitAttribute(units[i], "role")
    orgNames[i] <- unitAttribute(units[i], "titlewithpath")
  }

  list(
    name = rep(name, length(units)),
    fullName = rep(fullName, length(units)),
    phone = rep(phone, length(units)),
    email = rep(email, length(units)),
    group = groups,
    unit = units,
    org = orgs,
    role = roles,
    orgName = orgNames
  )
}

#' Get unit attributes from an access tree file
#'
#' Obtain organization unit attributes from an access tree JSON file
#'
#' @param unit Integer providing the look-up unit id
#' @param what Character string defining what to return for the given unit id
#' @param file Character string file name of the JSON file. Default values is
#'   NULL in which case the corresponding value from rapbaseConfig.yml will be
#'   used.
#' @param path Character string file path of the JSON file. Default value is
#'   \code{Sys.getenv("R_RAP_CONFIG_PATH")}.
#'
#' @return The corresponding value of 'what'.
#' @export
unitAttribute <- function(unit,
                          what,
                          file = NULL,
                          path = Sys.getenv("R_RAP_CONFIG_PATH")) {

  conf <- getConfig(fileName = "rapbaseConfig.yml")$accesstree

  if (is.null(file)) {
    file <- conf$file
  }

  stopifnot(file.exists(file.path(path, file)))
  if (!what %in% names(conf$list)) {
    stop(
      paste0(
        "Argument what is not one of '",
        paste0(names(conf$list), collapse = "', '"),
        "'"
      )
    )
  }

  d <- jsonlite::read_json(file.path(path, file)) %>%
    unlist()

  ind <- as.vector(d[names(d) == conf$list$unit]) == unit

  if (all(!ind)) {
    warning(
      paste0(
        "Unit '", unit, "' was not found! Hence, your request for '", what,
        "' will return empty!"
      )
    )
  }
  as.vector(d[names(d) == conf$list[[what]]][ind])
}


#' Get user attributes
#'
#' These are helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to these functions is made without any arguments.
#' If redefining contexts is needed, please use \code{\link{userInfo}} instead.
#'
#' @param shinySession A shiny session object. Default value is NULL
#' @param group Character string providing the name of the app R package name.
#'   The term "group" is used to relate to the environmental variable
#'   SHINYPROXY_USERGROUPS that corresponds to the apps a given user can access.
#'   Default value is NULL but should always be set when shiny app is run as a
#'   shinyproxy container.
#'
#' @return String with user attribute
#' @name userAttribute
#' @aliases getUserEmail getUserFullName getUserGroups getUserName getUserPhone
#'   getUserReshId getUserRole
#'
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' try(getUserEmail())
#' try(getUserEmail(shinySessionObject))
#' }
NULL

#' @rdname userAttribute
#' @export
getUserEmail <- function(shinySession = NULL, group = NULL) {
  userInfo(shinySession, entity = "email", group = group)
}


#' @rdname userAttribute
#' @export
getUserFullName <- function(shinySession = NULL, group = NULL) {
  userInfo(shinySession, entity = "full_name", group = group)
}


#' @rdname userAttribute
#' @export
getUserGroups <- function(shinySession = NULL, group = NULL) {
  userInfo(shinySession, entity = "groups", group = group)
}


#' @rdname userAttribute
#' @export
getUserName <- function(shinySession = NULL, group = NULL) {
  userInfo(shinySession, entity = "user", group = group)
}


#' @rdname userAttribute
#' @export
getUserPhone <- function(shinySession = NULL, group = NULL) {
  userInfo(shinySession, entity = "phone", group = group)
}


#' @rdname userAttribute
#' @export
getUserReshId <- function(shinySession = NULL, group = NULL) {
  userInfo(shinySession, entity = "resh_id", group = group)
}


#' @rdname userAttribute
#' @export
getUserRole <- function(shinySession = NULL, group = NULL) {
  userInfo(shinySession, entity = "role", group = group)
}
