% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3Classes.R
\name{original_file.archaeophases_plot}
\alias{original_file.archaeophases_plot}
\title{Check for an original \code{archaeophases_plot} file}
\usage{
\method{original_file}{archaeophases_plot}(x, file = NULL, ...)
}
\arguments{
\item{x}{An \code{archaeophases_plot} object.}

\item{file}{Either a path to a plot file, a connection,
or the value \code{clipboard()} to read from the system clipboard.}

\item{...}{Other parameters.}
}
\value{
A boolean, \code{TRUE} if the files match, \code{FALSE}
otherwise.
}
\description{
Checks whether or not a file is identical to the one used to create
an \code{archaeophases_plot} object.
}
\details{
If called with a single argument, checks the file indicated by
the \code{file_path} attribute.
}
\author{
Thomas S. Dye, \email{tsd@tsdye.online}
}
