#####  devianceGamma  #####
#' Function to calculate deviance for model predictions assuming a Gamma distribution.
#' @description This function calculates a deviance measure for model predictions assuming a Gamma distribution.
#' @usage devianceGamma(y, y_hat)
#' @param y a numeric vector of observations.
#' @param y_hat a numeric vector of predictions for y (must have same length as y.)
#' @return a numeric vector.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # n <- 1000
#' # 
#' # true <- runif(n, 100, 2000)
#' # observed <- rgamma(n, true)
#' # predicted <- exp(log(1050)/2 + log(true)/2 + rnorm(n, sd=0.3))
#' # 
#' # plot(observed, predicted)
#' # 
#' # devs <- devianceGamma(observed, predicted)
#' # sum(devs)
#' @export

devianceGamma <- function(y, y_hat){
  n <- length(y)
  if(length(y_hat) != n) stop("y and y_hat are not the same length")

  # Fix for very small values
  eps <- 1E-16
  y <- pmax(y, eps)
  y_hat <- pmax(y_hat, eps)
 
  devs <- -2*(log(y/y_hat) - (y-y_hat)/y_hat)

  return(devs)
}