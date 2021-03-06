% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{clean.file.path}
\alias{clean.file.path}
\title{Clean a file path.}
\usage{
clean.file.path(fpath)
}
\arguments{
\item{fpath}{File path to clean.}
}
\value{
Modified file path as julian_day/EVT_file_name.
}
\description{
Convert an EVT/OPP/VCT file path to a form suitable for storage in the SQLite
db. Any ".gz", ".opp", ".vct" extensions will be removed.
}
\examples{
\dontrun{
fpath <- clean.file.path("foo/2014_185/2014-07-04T00-00-02+00-00.opp.gz")
}
}
