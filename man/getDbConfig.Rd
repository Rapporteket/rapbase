% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbConnection.R
\name{getDbConfig}
\alias{getDbConfig}
\title{Get database connection configuration}
\usage{
getDbConfig(dbName = "data")
}
\arguments{
\item{dbName}{String providing the name of the database to connect to. If it
is "data" it will use the MYSQL_DB_DATA environment variable, if it is
"autoreport" it will use the MYSQL_DB_AUTOREPORT environment variable, and
if it is "raplog" it will use the MYSQL_DB_LOG environment variable. If
none of these are set, it will use the name provided.}
}
\value{
A list with name, user, password and host of the db connection.
}
\description{
Get database connection configuration
}
\keyword{internal}
