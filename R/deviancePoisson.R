#####  deviancePoisson  #####
#' Function to calculate deviance for model predictions assuming a Poisson distribution.
#' @description This function calculates a deviance measure for model predictions assuming a Poisson distribution.
#' @usage deviancePoisson(y, y_hat)
#' @param y a numeric vector of observations.
#' @param y_hat a numeric vector of predictions for y (must have same length as y.)
#' @return a numeric vector.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # n <- 1000
#' # 
#' # true <- runif(n, 0, 3)
#' # observed <- rpois(n, true)
#' # predicted <- exp(log(1.5)/2 + log(true)/2 + rnorm(n, sd=0.1))
#' # 
#' # plot(observed, predicted)
#' # 
#' # devs <- deviancePoisson(observed, predicted)
#' # sum(devs)
#' @export

deviancePoisson <- function(y, y_hat){
  n <- length(y)
  if(length(y_hat) != n) stop("y and y_hat are not the same length")
  
  # Separate into y > 0 and y == 0
  zeros <- which(y==0)
  
  devs <- vector(mode="numeric", length=n)
  devs[zeros] <- 2*y_hat[zeros]
  
  # Fix for very small values
  eps <- 1E-16
  y_hat <- pmax(y_hat, eps)
  
  if(length(zeros) > 0 ){
    devs[-zeros] <- 2*(y[-zeros]*log(y[-zeros]/y_hat[-zeros])-y[-zeros]+y_hat[-zeros])
  } else{
    devs <- 2*(y*log(y/y_hat)-y+y_hat)
  }

  return(devs)
}

y <- 200
y_hat <- 100
