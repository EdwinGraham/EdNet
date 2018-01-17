#####  devianceBernoulli  #####
#' Function to calculate deviance for model predictions assuming a Bernoulli distribution.
#' @description This function calculates a deviance measure for model predictions assuming a Bernoulli distribution.
#' @usage devianceBernoulli(Y, Y_hat)
#' @param Y a numeric vector of observations (0s and 1s.)
#' @param Y_hat a numeric vector of predictions (between 0 and 1) for Y (must have same length as Y.)
#' @return a numeric vector.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # n <- 1000
#' # 
#' # true <- 1/(1+exp(-rnorm(n)))
#' # observed <- rbinom(n, 1, true)
#' # predicted <- 1/(1+exp(-(rnorm(n, sd=0.1) + log(true/(1-true))/2 )))
#' # 
#' # plot(observed, predicted)
#' # 
#' # devs <- devianceBernoulli(observed, predicted)
#' # sum(devs)
#' @export

devianceBernoulli <- function(Y, Y_hat){
  n <- length(Y)
  if(length(Y_hat) != n) stop("Y and Y_hat are not the same length")
  
  # Fix for very small values
  eps <- 1E-16
  Y_hat <- pmin(pmax(Y_hat, eps), 1-eps)
  
  devs <- -2*(Y*log(Y_hat) + (1-Y)*log(1-Y_hat))

  return(devs)
}