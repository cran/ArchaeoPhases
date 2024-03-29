% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllenDataFrame.R
\name{allen_string_to_set}
\alias{allen_string_to_set}
\title{Convert a string containing Allen relation codes to a relation set}
\usage{
allen_string_to_set(s)
}
\arguments{
\item{s}{A string with Allen relation codes.}
}
\value{
A vector of single letter Allen relation codes.
}
\description{
Characters in the string that are not Allen relation codes are
not identified and are added to the set.
}
\author{
Thomas S. Dye
}
