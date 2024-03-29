% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllenDataFrame.R
\name{allen_relate_intervals}
\alias{allen_relate_intervals}
\title{Relate two or more observed intervals}
\usage{
allen_relate_intervals(mcmc, chains)
}
\arguments{
\item{mcmc}{Dataframe or archaeophases_mcmc object with the MCMC output
from a Bayesian calibration.}

\item{chains}{a list of lists, each with two named elements,
each element a vector of names or indexes of columns in \code{mcmc}.}
}
\value{
A dataframe suitable for plotting with \code{allen_plot}.
}
\description{
Reads MCMC output to create a dataframe suitable
for plotting the observed Allen relation of two
intervals.
}
\author{
Thomas S. Dye
}
