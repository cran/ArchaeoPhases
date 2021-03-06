% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3Classes.R
\name{new_archaeophases_plot}
\alias{new_archaeophases_plot}
\title{Constructor for \code{archaeophases_plot} object}
\usage{
new_archaeophases_plot(x = list(), mcmc = list(), call = match.call())
}
\arguments{
\item{x}{A data frame with the plot data.}

\item{mcmc}{An \code{archaeophases_mcmc} object.}

\item{call}{How the function was called.}
}
\value{
An \code{archaeophases_plot} object that inherits from
\code{archaeophases_mcmc}.
}
\description{
Objects returned by ArchaeoPhases plot functions.
}
\seealso{
\code{\link{read_chronomodel}}

\code{\link{read_bcal}}

\code{\link{read_oxcal}}
}
\author{
Thomas S. Dye, \email{tsd@tsdye.online}
}
