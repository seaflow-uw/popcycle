% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta.R
\name{csv_convert}
\alias{csv_convert}
\title{Convert data from sqlite3 database into a csv file of curated SeaFlow data for a cruise, along with metadata files.}
\usage{
csv_convert(db, meta, path, cruisename, version)
}
\arguments{
\item{db}{SQLite3 database file path.}

\item{meta}{metadata containing cruise ID (provided by googlesheet "SeaFlow log instrument")}

\item{path}{Path to save the file.}

\item{version}{Version of the dataset.}
}
\value{
None
}
\description{
Convert data from sqlite3 database into a csv file of curated SeaFlow data for a cruise, along with metadata files.
}
\examples{
\dontrun{
# load metadata to get offical cruise name
 meta <- googlesheets4::range_read('https://docs.google.com/spreadsheets/d/1Tsi7OWIZWfCQJqLDpId2aG_i-8Cp-p63PYjjvDkOtH4/edit#gid=0')
csv_convert(db,meta, path)
}
}
