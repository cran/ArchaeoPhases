% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllenDataFrame.R
\name{allen_proportion_result}
\alias{allen_proportion_result}
\title{Calculate the proportion of each relation in a result vector}
\usage{
allen_proportion_result(result_vector, sort = FALSE)
}
\arguments{
\item{result_vector}{A result vector}

\item{sort}{if TRUE sort in decreasing order else return unsorted vector}
}
\value{
A named vector with proportions
}
\description{
Divides through by the sum of observations in the result vector.
Assigns the names of the result vector to the optionally sorted
return vector.
}
\author{
Thomas S. Dye
}
