% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time.R
\name{RFC3339.now}
\alias{RFC3339.now}
\title{Get current UTC datetime as RFC3339 string suitable for entry into db}
\usage{
RFC3339.now()
}
\value{
Date text as YYYY-MM-DDTHH:MM:SS+00:00."
}
\description{
Get current UTC datetime as RFC3339 string suitable for entry into db
}
\examples{
\dontrun{
datetime.str <- RFC3339.now()
}
}
