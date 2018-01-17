#####  devianceGamma  #####
#' Function to calculate deviance for model predictions assuming a Gamma distribution.
#' @description This function calculates a deviance measure for model predictions assuming a Gamma distribution.
#' @usage devianceGamma(Y, Y_hat)
#' @param Y a numeric vector of observations.
#' @param Y_hat a numeric vector of predictions for Y (must have same length as Y.)
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

devianceGamma <- function(Y, Y_hat){
  n <- length(Y)
  if(length(Y_hat) != n) stop("Y and Y_hat are not the same length")

  # Fix for very small values
  eps <- 1E-16
  Y <- pmax(Y, eps)
  Y_hat <- pmax(Y_hat, eps)
 
  devs <- -2*(log(Y/Y_hat) - (Y-Y_hat)/Y_hat)

  return(devs)
}