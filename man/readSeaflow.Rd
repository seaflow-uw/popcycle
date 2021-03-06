% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Load.R
\name{readSeaflow}
\alias{readSeaflow}
\title{Read an EVT or OPP binary file.}
\usage{
readSeaflow(
  path,
  count.only = FALSE,
  transform = TRUE,
  channel = NULL,
  columns = NULL
)
}
\arguments{
\item{path}{Path to binary file. If this file does not exist the same
file ending with ".gz" will also be checked.}

\item{count.only}{Only return the count of particles from the file header.}

\item{transform}{Convert log data to linear.}

\item{channel}{Only return data for channels listed here. Can be a single
channel name or a vector of channel names.}
}
\value{
Data frame of particle data or number of particles.
}
\description{
Read a SeaFlow LabView binary particle data file. This file may be gzipped.
}
\examples{
\dontrun{
evt <- readSeaflow("foo/2014_213/2014-07-04T00-00-02+00-00.gz", channel=c("fsc_small", "pe"))
}
}
