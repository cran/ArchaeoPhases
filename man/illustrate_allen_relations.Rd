% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllenDataFrame.R
\name{illustrate_allen_relations}
\alias{illustrate_allen_relations}
\title{Data for an illustrative graphic}
\usage{
illustrate_allen_relations(relations = "basic")
}
\arguments{
\item{relations}{One of 'basic', 'concurrent', 'distinct', 'stratigraphic', 'branching', 'transformation', 'reticulation', 'sequence', 'branch', 'transform', or 'reticulate'.}
}
\value{
A dataframe for input to \code{allen_plot}
}
\description{
Create a dataframe that can be used as input for an illustrative plot.
Useful for describing the Allen operators: illustrate the full
set of Allen relations, concurrent Allen relations, and relations with
distinct endpoints.  Also, useful for describing the
chronological domains of stratification, branching, transformation, and
reticulation.
}
\details{
The illustrative graphics include:
\describe{
\item{basic}{the 13 basic Allen relations (default);}
\item{concurrent}{concurrent relations;}
\item{distinct}{relations with distinct endpoints;}
\item{stratigraphic}{basic relations established by an observation of superposition;}
\item{branching}{basic branching relations;}
\item{transformation}{basic relations of transformation;}
\item{reticulation}{basic relations of reticulation;}
\item{sequence}{composite relations in a stratigraphic sequence;}
\item{branch}{composite relations of branching;}
\item{transform}{composite relations of transformation; or}
\item{reticulate}{composite relations of reticulation.}
}
}
\references{
Harris, E. \emph{Principles of Archaeological Stratigraphy}. Second edition.
London: Academic Press.

Lyman, R. Lee and Michael J. O'Brien.  Seriation and cladistics: The
difference between anagenetic and cladogenetic evolution.  Chapter 5 in
\emph{Mapping Our Ancestors: Phylogenetic Approaches in Anthropology and
Prehistory.} New Brunswick: AldineTransaction.

Viola, Tullio.  \emph{Peirce on the Uses of History.}  Berlin: de Gruyter.
See chapter 3, Historicity as Process, especially p. 83--88.
}
\author{
Thomas S. Dye
}
