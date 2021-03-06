

#####################################################
#         Estimation of Credible interval           #
#####################################################

#' Bayesian credible interval for a series of dates
#'
#' Estimation of the shortest credible interval for each variable of a simulated Markov chain
#'
#' @details A \eqn{(100 * level)}\% credible interval is an interval that keeps \eqn{N * (1 -level)} elements of the sample outside the interval.
#' The \eqn{(100*level)}\% credible interval is the shortest of the intervals
#' @param data data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest.
#' @param level Probability corresponding to the level of confidence used
#' for the credible interval.
#' @param roundingOfValue Integer indicating the number of decimal places.
#'
#' @return Returns a matrix of values containing the level of confidence and the endpoints of
#' the shortest credible interval for each variable of the MCMC chain. The name of the
#' resulting rows are the positions of the corresponding columns in the CSV file. The
#' result is given in calendar years (BC/AD).
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Events)
#'   MultiCredibleInterval(Events, c(2, 4, 3), 0.95)
#'
#' @export
MultiCredibleInterval <- function(data, position, level=0.95, roundingOfValue=0){

  # number of chains
  L = length(position)

  # matrix of results for each chain
  result = matrix(nrow=L, ncol=3)

  colnames(result) <- c("Level","Credible Interval Inf", "Credible Interval Sup")

  # names
  rownames(result) <- names(data)[position]

  for (i in 1:L) {

    sorted_sample <- sort(data[,position[i]])     # ordering the sample
    N = length(sorted_sample)                     # calculation of the sample size of the chain
    OutSample = N * (1-level)          # calculation of the number of data to be outside the interval

    I =  cbind(sorted_sample[1:(OutSample+1)] , sorted_sample[(N-OutSample):N])    #   combinasion of all credible intervals

    l = I[,2]-I[,1]   # length of intervals
    j <- which.min(l) # look for the shortest interval

    result[i,] =   c(level, round(I[j,1], digits = roundingOfValue), round(I[j,2],digits = roundingOfValue) )   # returns the level and the endpoints

  }
  return(result)
}

#' Bayesian credible interval for a series of dates
#'
#' Estimate the shortest credible interval for each of several
#' MCMC chains.
#'
#' @details A \eqn{(100 * level)}\% credible interval is an interval
#' that keeps \eqn{N * (1 -level)} elements of the sample outside the interval.
#' The \eqn{(100*level)}\% credible interval is the shortest of the intervals.
#'
#' @param data data frame containing the output of the MCMC algorithm.
#' @param position Numeric vector containing the position of the column
#' corresponding to the MCMC chains of interest, or a list of column names.
#' @param level Probability corresponding to the level of confidence used
#' for the credible interval.
#' @param round_to Integer indicating the number of decimal places.
#'
#' @return Returns a list with the following components:
#'
#' \describe{
#' \item{ci}{A data frame with a row for each column in \code{data} and two
#' columns: \code{inf}, the lower credible interval in calendar years (BC/AD);
#' and \code{sup}, the upper credible interval in calendar years (BC/AD).}
#'
#' \item{level}{Probability corresponding to the level of confidence used
#' for the credible interval.}
#' \item{call}{The function call.}
#' }
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#'
#' @author Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}, and
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}.
#'
#' @examples
#'   data(Events)
#'   multi_credible_interval(Events, c(2, 4, 3), 0.95)
#'   # round to decade
#'   multi_credible_interval(Events, c(2, 4, 3), 0.95, -1)
#'
#' @export
multi_credible_interval <- function(data,
                                    position,
                                    level = 0.95,
                                    round_to = 0) {
    res <- apply(X = data[, position],
                 MARGIN = 2,
                 FUN = function(x, l = level, r = round_to) {
                     ret <- credible_interval(x, l, r)
                     ret$ci
                 })
    list(ci = t(res), level = level, call = match.call())
}
