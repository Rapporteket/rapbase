% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetUserReshId.R
\name{getUserReshId}
\alias{getUserReshId}
\title{Get user resh ID from config or session object}
\usage{
getUserReshId(shinySession = NULL)
}
\arguments{
\item{shinySession}{Shiny session object (list, NULL by default). Must be
provided when the source of user attributes is either the shiny app url or
an external authentication provider. By default this will apply to the
'TEST', 'QA' and 'PRODUCTION' contexts in which case the shiny session
object must be provided.}
}
\value{
String user name
}
\description{
This is a helper function for \code{\link{userInfo}}. When used without a
shiny session object calls to this function is made without any arguments. If
redefining contexts is needed, please use \code{\link{userInfo}} instead.
}
\examples{
\donttest{
# Requires a valid shiny session object
getUserReshId()
getUserReshId(shinySessionObject)
}

}
\seealso{
\code{\link{getUserName}},
\code{\link{getUserGroups}}, \code{\link{getUserRole}}
}
