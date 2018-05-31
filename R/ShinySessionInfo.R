shinySessionInfo <- function(shinySession, entity, testCase = FALSE) {
  
  if (is.null(shinySession)) {
    stop("No session information provided. Cannot do anything")
  }
  
  if (testCase) {
    warning("This is a test. Not to be applied in production!",
            immediate. = TRUE)
    us <- shinySession$clientData$url_search
    user <- us$`X-USER`
    groups <- us$`X-GROUPS`
    resh_id <- us$resh_id
    role <- us$role
  } else {
    user <- shinySession$user
    groups <- shinySession$groups
    resh_id <- shinySession$HTTP_RESH_ID
    role <- shinySession$HTTP_ROLE
  }
  
  switch(entity,
         user = user,
         groups = groups,
         resh_id = resh_id,
         role = role)
  
}