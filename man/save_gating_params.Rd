% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{save_gating_params}
\alias{save_gating_params}
\title{Save gating parameters.}
\usage{
save_gating_params(db, gates.log, gating_id = NULL)
}
\arguments{
\item{db}{SQLite3 database file path.}

\item{gates.log}{Named list of per population classification parameters.}

\item{gating_id}{Optional, supply a gating ID. If not provided a UUID string
will be generated.}
}
\value{
Database gating ID string.
}
\description{
This creates a set per population entries in the gating table and saves any
manual gating polygon coordinates in the poly table.
}
