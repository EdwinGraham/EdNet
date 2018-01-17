#####  devianceBernoulli  #####
#' Function to calculate deviance for model predictions assuming a Bernoulli distribution.
#' @description This function calculates a deviance measure for model predictions assuming a Bernoulli distribution.
#' @usage devianceBernoulli(y, y_hat)
#' @param y a numeric vector of observations (0s and 1s.)
#' @param y_hat a numeric vector of predictions (between 0 and 1) for y (must have same length as y.)
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

devianceBernoulli <- function(y, y_hat){
  n <- length(y)
  if(length(y_hat) != n) stop("y and y_hat are not the same length")
  
  # Fix for very small values
  eps <- 1E-16
  y_hat <- pmin(pmax(y_hat, eps), 1-eps)
  
  devs <- -2*(y*log(y_hat) + (1-y)*log(1-y_hat))

  return(devs)
}