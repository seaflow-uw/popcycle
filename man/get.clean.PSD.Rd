% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{get.clean.PSD}
\alias{get.clean.PSD}
\title{Clean the Particle size disitrubution by selecting the closest refractive index to the reference diameter for a particular population,
keep only the clean data (flag ==0 )}
\usage{
get.clean.PSD(PSD, pop = "prochloro", ref_diam = 0.54)
}
\arguments{
\item{PSD}{Particle size disitribution created by create_PSD().}

\item{pop}{Name of the population. Can be either "prochloro", "synecho", "picoeuk", "croco"}

\item{ref_diam}{Diameter value (in micron)}
}
\value{
A cleaned Particle Size distribution with corrected refractive index
}
\description{
Clean the Particle size disitrubution by selecting the closest refractive index to the reference diameter for a particular population,
keep only the clean data (flag ==0 )
}
