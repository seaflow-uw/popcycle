% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Load.R
\name{untransformData}
\alias{untransformData}
\title{Log SeaFlow linear data}
\usage{
untransformData(df, columns = NULL)
}
\arguments{
\item{df}{Data frame returned by readSeaflow}

\item{columns}{If not NULL, only these columns will be modified}
}
\value{
Exponentiated version of df
\dontrun{
evt <- untransformData(evt)
}
}
\description{
Performs the reverse operation of transformData, converting exponentiated
data back to its original form of log data on a 2^16 linear scale.
}
