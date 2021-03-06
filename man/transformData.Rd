% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Load.R
\name{transformData}
\alias{transformData}
\title{Exponentiate SeaFlow log data}
\usage{
transformData(df, columns = NULL)
}
\arguments{
\item{df}{Data frame returned by readSeaflow}

\item{columns}{If not NULL, only these columns will be modified}
}
\value{
Exponentiated version of integer.dataframe
}
\description{
SeaFlow data is stored as 3.5 decades of log data on a linear 16-bit scale.
This function will exponentiate this data to a linear scale between 1 and
10^3.5.
}
\examples{
\dontrun{
evt <- transformData(evt)
}
}
