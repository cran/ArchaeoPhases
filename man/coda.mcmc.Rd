\name{coda.mcmc}
\alias{coda.mcmc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a \code{mcmc.list} object for CODA users.
}
\description{
This is a wrapper function that extracts parallel chains from the data given and coerces the
  output to a single \code{mcmc.list} object and diagnostic tools from "coda" can be used. 
}
\usage{
coda.mcmc(data, numberChains=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{dataframe containing the output of the MCMC algorithm}
  \item{numberChains}{number of parallel chains. Default = 1.}
}
\value{
 A \code{mcmc.list} object.
}
\author{
  Anne Philippe <Anne.Philippe@univ-nantes.fr> and
  
  Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}

\examples{
    data(Events)
    
    # Delete the first column containing the iteration number
    mcmcList = coda.mcmc(Events[,-1], numberChains = 3) 
    plot(mcmcList)
    gelman.diag(mcmcList)
    
    # Delete the first column containing the iteration number
    mcmcList = coda.mcmc(Phases[,-1], numberChains = 3) 
    plot(mcmcList)
    # The multivariate criterion can not be evaluated when a phase 
    # contains only one date. This induces colinearity problems. 
    gelman.diag(mcmcList, multivariate=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{MCMC list}

