#' Provide user attributes based on environment context
#'
#' Extracts elements from either config, url (shiny) or session (shiny)
#' relevant for user data such as name, group, role and reshId. Source of info
#' is based on environment context and can be controlled by altering the default
#' settings for which contexts that will apply for the various sources of user
#' data. This function will normally be used via its helper functions (see
#' below).
#'
#' @param entity String defining the element to return. Currently, one of
#' 'user', groups', 'resh_id', 'role', 'email', 'full_name' or 'phone'
#' @param shinySession Shiny session object (list, NULL by default). Must be
#' provided when the source of user attributes is either the shiny app url or
#' an external authentication provider. By default this will apply to the
#' 'TEST', 'QA' and 'PRODUCTION' contexts in which case the shiny session
#' object must be provided.
#' @param devContexts A character vector providing unique instances to be
#' regarded as a development context. In this context user attributes will be
#' read from configuration as provided by 'rapbaseConfig.yml'. The instances
#' provided cannot overlap instances provided in any other contexts. By default
#' set to \code{c("DEV")}.
#' @param testContexts A character vector providing unique instances to be
#' regarded as a test context. In this context user attributes will be read
#' from the url call to a shiny application. Hence, for this context the
#' corresponding shiny session object must also be provided. The instances
#' provided cannot overlap instances provided in any other contexts. By default
#' set to \code{c("TEST")}.
#' @param prodContexts A character vector providing unique instances to be
#' regarded as a production context. In this context user attributes will be
#' read from the shiny session object (as shiny server interacts with an
#' external log-in service). Hence, for this context the corresponding shiny
#' session object must also be provided. The instances provided cannot overlap
#' instances provided in any other contexts. By default set to
#' \code{c("QA", "PRODUCTION")}.
#'
#' @return String of single user data element
#'
#' @seealso \code{\link{getUserName}}, \code{\link{getUserGroups}},
#' \code{\link{getUserReshId}}, \code{\link{getUserRole}}
#'
#' @export

userInfo <- function(entity, shinySession = NULL, devContexts = c("DEV"),
                     testContexts = c("TEST"),
                     prodContexts = c("QA", "PRODUCTION")) {

  # check for valid entities
  if (!(entity %in% c(
    "user", "groups", "resh_id", "role", "email",
    "full_name", "phone"
  ))) {
    stop("Incorrect entity provided! Must be one of 'user', 'groups', 'resh_id'
         'role' or 'email'")
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
    if (is.null(shinySession)) {
      stop("Session information is empty! Eventually, that will come bite you")
    }

    if (!any(c("ShinySession", "session_proxy", "MockShinySession") %in%
      attributes(shinySession)$class)) {
      stop(paste(
        "Got no object of class 'ShinySession' or 'session_proxy'!",
        "Your carma is way below threshold..."
      ))
    }

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

  if (context %in% testContexts || context %in% prodContexts) {
    if (is.null(shinySession)) {
      stop("Session information is empty!. Cannot do anything")
    }

    if (!any(c("ShinySession", "session_proxy") %in%
      attributes(shinySession)$class)) {
      stop(paste(
        "Got no object of class 'ShinySession' or 'session_proxy'!",
        "Cannot do anything"
      ))
    }

    if (context %in% testContexts) {
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
      user <- shinySession$user
      groups <- shinySession$groups
      resh_id <- shinySession$request$HTTP_RESHID
      role <- shinySession$request$HTTP_ROLE
      email <- shinySession$request$HTTP_EMAIL
      full_name <-
        parse(text = paste0("'", shinySession$request$HTTP_FULLNAME, "'"))[[1]]
      phone <- shinySession$request$HTTP_PHONE
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


#' Get user email from config or session object
#'
#' This is a helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to this function is made without any arguments. If
#' redefining contexts is needed, please use \code{\link{userInfo}} instead.
#'
#' @inheritParams userInfo
#'
#' @return String email address
#'
#' @seealso \code{\link{getUserName}},
#' \code{\link{getUserGroups}}, \code{\link{getUserReshId}},
#' \code{\link{getUserEmail}}
#'
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' try(getUserEmail())
#' try(getUserEmail(shinySessionObject))
#' }
#'
#' @export


getUserEmail <- function(shinySession = NULL) {
  userInfo(shinySession, entity = "email")
}


#' Get user full name from config or session object
#'
#' This is a helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to this function is made without any arguments. If
#' redefining contexts is needed, please use \code{\link{userInfo}} instead.
#'
#' @inheritParams userInfo
#'
#' @return String full name
#'
#' @seealso \code{\link{getUserName}},
#' \code{\link{getUserGroups}}, \code{\link{getUserReshId}},
#' \code{\link{getUserEmail}}, \code{\link{getUserPhone}}
#'
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' try(getUserFullName())
#' try(getUserFullName(shinySessionObject))
#' }
#'
#' @export


getUserFullName <- function(shinySession = NULL) {
  userInfo(shinySession, entity = "full_name")
}


#' Get user groups from config or session object
#'
#' This is a helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to this function is made without any arguments. If
#' redefining contexts is needed, please use \code{\link{userInfo}} instead.
#'
#' @inheritParams userInfo
#'
#' @return String user name
#'
#' @seealso \code{\link{getUserName}},
#' \code{\link{getUserReshId}}, \code{\link{getUserRole}}
#'
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' try(getUserGroups())
#' try(getUserGroups(shinySessionObject))
#' }
#'
#' @export


getUserGroups <- function(shinySession = NULL) {
  userInfo(shinySession, entity = "groups")
}


#' Get user name from config or session object
#'
#' This is a helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to this function is made without any arguments. If
#' redefining contexts is needed, please use \code{\link{userInfo}} instead.
#'
#' @inheritParams userInfo
#'
#' @return String user name
#'
#' @seealso \code{\link{getUserGroups}},
#' \code{\link{getUserReshId}}, \code{\link{getUserRole}}
#'
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' try(getUserName())
#' try(getUserName(shinySessionObject))
#' }
#'
#' @export


getUserName <- function(shinySession = NULL) {
  userInfo(shinySession, entity = "user")
}


#' Get user phone (number) from config or session object
#'
#' This is a helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to this function is made without any arguments. If
#' redefining contexts is needed, please use \code{\link{userInfo}} instead.
#'
#' @inheritParams userInfo
#'
#' @return String phone number
#'
#' @seealso \code{\link{getUserName}},
#' \code{\link{getUserGroups}}, \code{\link{getUserReshId}},
#' \code{\link{getUserEmail}}, \code{\link{getUserFullName}}
#'
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' try(getUserPhone())
#' try(getUserPhone(shinySessionObject))
#' }
#'
#' @export


getUserPhone <- function(shinySession = NULL) {
  userInfo(shinySession, entity = "phone")
}


#' Get user resh ID from config or session object
#'
#' This is a helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to this function is made without any arguments. If
#' redefining contexts is needed, please use \code{\link{userInfo}} instead.
#'
#' @inheritParams userInfo
#'
#' @return String user name
#'
#' @seealso \code{\link{getUserName}},
#' \code{\link{getUserGroups}}, \code{\link{getUserRole}}
#'
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' try(getUserReshId())
#' try(getUserReshId(shinySessionObject))
#' }
#'
#' @export


getUserReshId <- function(shinySession = NULL) {
  userInfo(shinySession, entity = "resh_id")
}


#' Get user role from config or session object
#'
#' This is a helper function for \code{\link{userInfo}}. When used without a
#' shiny session object calls to this function is made without any arguments. If
#' redefining contexts is needed, please use \code{\link{userInfo}} instead.
#'
#' @inheritParams userInfo
#'
#' @return String user name
#'
#' @seealso \code{\link{getUserName}},
#' \code{\link{getUserGroups}}, \code{\link{getUserReshId}}
#'
#' @examples
#' \donttest{
#' # Requires a valid shiny session object
#' try(getUserRole())
#' try(getUserRole(shinySessionObject))
#' }
#'
#' @export


getUserRole <- function(shinySession = NULL) {
  userInfo(shinySession, entity = "role")
}
