% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gating.R
\name{gating.csv.to.poly.log}
\alias{gating.csv.to.poly.log}
\title{Import old popcycle gating logs.}
\usage{
gating.csv.to.poly.log(csv.files, poly.log = NULL)
}
\arguments{
\item{csv.files}{CSV log files created by previous versions of popcycle}

\item{poly.log}{Named list of gating polygon definitions. Existing population
gates will be updated. otherwise new ones will be appended to the list. If
poly.log is NULL a new list will be created.}
}
\value{
poly.log gating polygon definitions.
}
\description{
Convert a list of old popcycle CSV gating log files into a poly.log
data structure identical to that produced by set.gating.params().
}
\examples{
\dontrun{
poly.log <- gating.csv.to.poly.log(gates.csv.files)
}
}
