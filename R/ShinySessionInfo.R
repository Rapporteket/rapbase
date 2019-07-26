#' Provide elements from a shiny session object
#' 
#' Extracts elements from a shiny session object relevant for user data. This
#' function will normally be used via its helper functions.
#'
#' @param shinySession List shiny session object
#' @param entity String defining the element to return. One of 'user',
#' 'groups', 'resh_id' or 'role'
#' @param testCase Logical defining to use shinySessionInfo on a test case.
#' For test cases user privileges are provided by a query defined in the
#' url of the shiny application request. Default FALSE
#'
#' @return String of single user data element
#' 
#' @seealso \code{\link{getShinyUserName}}, \code{\link{getShinyUserGroups}},
#' \code{\link{getShinyUserReshId}}, \code{\link{getShinyUserRole}}
#' 
#' @export

shinySessionInfo <- function(shinySession, entity, testCase = FALSE) {
  
  if (is.null(shinySession)) {
    stop("Session information is empty!. Cannot do anything")
  }
  
  if (!c("ShinySession") %in% attributes(shinySession)$class) {
    stop("Got no object of class 'ShinySession'! Cannot do anything")
  }
  
  if (!(entity %in% c("user", "groups", "resh_id", "role"))) {
    stop("Incorrect entity provided! Must be one of 'user', 'groups', 'resh_id'
         or 'role'")
  }
  
  if (testCase) {
    #warning("This is a test. Not to be applied in production!",
    #        immediate. = TRUE)
    us <- shiny::parseQueryString(shinySession$clientData$url_search)
    user <- us$`X-USER`
    groups <- us$`X-GROUPS`
    resh_id <- us$resh_id
    role <- us$role
  } else {
    user <- shinySession$user
    groups <- shinySession$groups
    resh_id <- shinySession$request$HTTP_RESH_ID
    role <- shinySession$request$HTTP_ROLE
  }
  
  switch(entity,
         user = user,
         groups = groups,
         resh_id = resh_id,
         role = role)
  
}