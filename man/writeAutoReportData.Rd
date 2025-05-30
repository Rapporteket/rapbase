% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoReport.R
\name{writeAutoReportData}
\alias{writeAutoReportData}
\title{Write automated report metadata}
\usage{
writeAutoReportData(config)
}
\arguments{
\item{config}{a list of yaml configuration}
}
\description{
Write automated report metadata
}
\examples{
\donttest{
# Example depend on environment variable R_RAP_CONFIG_PATH being set
try(config <- readAutoReportData())
try(writeAutoReportData(config = config))
}

}
