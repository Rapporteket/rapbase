% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stagingData.R
\name{stagingData}
\alias{stagingData}
\alias{listStagingData}
\alias{mtimeStagingData}
\alias{saveStagingData}
\alias{loadStagingData}
\alias{deleteStagingData}
\alias{cleanStagingData}
\title{Staging data functions}
\usage{
listStagingData(registryName)

mtimeStagingData(registryName)

saveStagingData(registryName, dataName, data)

loadStagingData(registryName, dataName)

deleteStagingData(registryName, dataName)

cleanStagingData(eolAge, dryRun = TRUE)
}
\arguments{
\item{registryName}{Character string providing the registry name.}

\item{dataName}{Character string providing the data set name.}

\item{data}{A data object such as a data.frame to be stored as
\code{dataName}.}

\item{eolAge}{Numeric providing the staging data end-of-life age in seconds.
Based on the current time and the time of storage staging files
older than \code{eolAge} will be identified as subject for removal.}

\item{dryRun}{Logical defining if function is to be run in dry (none
destructive) mode.}
}
\value{
\itemize{
  \item \code{listStagingData()} returns a character vector of staging data
    sets for the given registry (\code{registryName}).
  \item \code{mtimeStagingData()} returns a staging data set named POSIXct
    vector of modification times for the given registry
    (\code{registryName}).
  \item \code{saveStagingData()} when successful returns the data object
    (\code{data}), invisibly. If saving fails a warning is issued and the
    function returns FALSE.
  \item \code{loadStagingData()} returns the data object corresponding to
    the name given upon saving (\code{dataName}). If the requested data set
    for loading does not exist the function returns FALSE.
  \item \code{deleteStagingData()} returns TRUE if the data set was deleted
    and FALSE if not.
  \item \code{cleanStagingData()} returns a list of data sets (to be)
    removed.
}
}
\description{
Low level functions for handling registry staging data at Rapporteket. As
such, these functions does not provide staging data management \emph{per se}.
Proper management, \emph{e.g.} staging data updates and fallback logic
must therefore be established within each registry that take staging data
into use.
}
\details{
Staging data is stored as binary large objects in a database.
A per registry symmetric encryption of storage
content is enforced. Keys used for encryption are generated from existing
database credentials. Therefore, please note that removing or changing
such credentials will render any historic staging data inaccessible.

\code{cleanStagingData()} globally removes all staging data with store date
prior to the end-of-life age provided. This is a vastly destructive function
that should be used with great care.
}
\examples{
\donttest{
## Prep test data
registryName <- "rapbase"
dataName <- "testData"
data <- mtcars

## Save data for staging
try(saveStagingData(registryName, dataName, data))

## List data currently in staging
try(listStagingData(registryName))

## Retrieve data set from staging and compare to outset
try(stagedData <- loadStagingData(registryName, dataName))
try(identical(data, stagedData))

## Get modification time for staging file(s)
try(mtimeStagingData(registryName))
}
}
