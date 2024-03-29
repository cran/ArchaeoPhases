% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImportCSV.R
\name{read_bcal}
\alias{read_bcal}
\title{Read MCMC output from BCal}
\usage{
read_bcal(file, bin_width = 1, quiet = "no")
}
\arguments{
\item{file}{Either a path to a CSV file, a connection,
or the value \code{clipboard()} to read from the system clipboard.
The CSV file can be compressed or plain.
See \code{\link[readr]{read_csv}} for details.}

\item{bin_width}{The bin width specified for the
\href{https://bcal.shef.ac.uk/}{BCal} calibration.
Defaults to the \href{https://bcal.shef.ac.uk/}{BCal} default of 1.}

\item{quiet}{One of "no" (default) to allow messages and warnings, "partial"
to suppress messages and allow warnings, or "yes" to suppress messages
and warnings.}
}
\value{
An \code{archaeophases_mcmc} object containing the marginal
posterior(s) as a data frame, or NULL if \code{file} is not found.
}
\description{
Import a CSV file containing the output of the MCMC algorithm produced
by \href{https://bcal.shef.ac.uk/}{BCal}.
}
\details{
The \code{read_bcal} function is built on \code{\link[readr]{read_csv}}.
It aims to be fast and simple, and to return the marginal posteriors free
of extraneous artifacts.  The iteration column in the CSV file is discarded,
as are an empty last column and an empty last row.
}
\examples{

\dontrun{
  # Import of MCMC output from BCal
  data(Fishpond)
  write.csv(Fishpond, "fishpond_MCMC.csv", row.names=FALSE)
  fishpond <- read_bcal("fishpond_MCMC.csv")

# Read from connection
  bc_1 <- read_bcal("http://tsdye.online/AP/bc-1.csv")
  bc_17 <- read_bcal("http://tsdye.online/AP/bc-17.csv", bin_width = 17)
}

}
\seealso{
\code{\link[readr]{read_csv}}

\code{\link{ImportCSV}}

\code{\link{new_archaeophases_mcmc}}
}
\author{
Thomas S. Dye, \email{tsd@tsdye.online}
}
