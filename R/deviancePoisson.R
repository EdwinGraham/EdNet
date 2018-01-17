#####  deviancePoisson  #####
#' Function to calculate deviance for model predictions assuming a Poisson distribution.
#' @description This function calculates a deviance measure for model predictions assuming a Poisson distribution.
#' @usage deviancePoisson(Y, Y_hat)
#' @param Y a numeric vector of observations.
#' @param Y_hat a numeric vector of predictions for Y (must have same length as Y.)
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

deviancePoisson <- function(Y, Y_hat){
  n <- length(Y)
  if(length(Y_hat) != n) stop("Y and Y_hat are not the same length")
  
  # Separate into Y > 0 and Y == 0
  zeros <- which(Y==0)
  
  devs <- vector(mode="numeric", length=n)
  devs[zeros] <- 2*Y_hat[zeros]
  
  # Fix for very small values
  eps <- 1E-16
  Y_hat <- pmax(Y_hat, eps)
  
  if(length(zeros) > 0 ){
    devs[-zeros] <- 2*(Y[-zeros]*log(Y[-zeros]/Y_hat[-zeros])-Y[-zeros]+Y_hat[-zeros])
  } else{
    devs <- 2*(Y*log(Y/Y_hat)-Y+Y_hat)
  }

  return(devs)
}