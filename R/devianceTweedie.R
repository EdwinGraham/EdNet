#####  devianceTweedie  #####
#' Function to calculate deviance for model predictions assuming a Tweedie distribution.
#' @description This function calculates a deviance measure for model predictions assuming a Tweedie distribution.
#' @usage devianceTweedie(Y, Y_hat, p=1.5)
#' @param Y a numeric vector of observations.
#' @param Y_hat a numeric vector of predictions for Y (must have same length as Y.)
#' @param p Tweedie distribution variance power parameter
#' @return a numeric vector.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # n <- 1000
#' # 
#' # true <- rgamma(n, shape=15, scale=10)
#' # observed <- rtweedie(n, true, phi=50, p=1.5)
#' # predicted <- exp(log(150)/3 + 2*log(true)/3 + rnorm(n, sd=0.1))
#' # 
#' # plot(observed, predicted)
#' # 
#' # devs <- devianceTweedie(observed, predicted)
#' # sum(devs)
#' @export

devianceTweedie <- function(Y, Y_hat, p=1.5){
  n <- length(Y)
  if(length(Y_hat) != n) stop("Y and Y_hat are not the same length")

  if(p<=1 | p>=2) stop("p should be between 1 and 2")
  
  # Separate into Y > 0 and Y == 0
  zeros <- which(Y==0)
 
  devs <- vector(mode="numeric", length=n)
  devs[zeros] <-  2*Y_hat[zeros]^(2-p)/(2-p)
  
  # Fix for verY small values
  eps <- 1E-16
  Y_hat <- pmax(Y_hat, eps)
  
  if(length(zeros) > 0 ){
    devs[-zeros] <- 2*(Y[-zeros]*(Y[-zeros]^(1-p) - Y_hat[-zeros]^(1-p))/(1-p) - (Y[-zeros]^(2-p) - Y_hat[-zeros]^(2-p))/(2-p))
  } else{
    devs <- 2*(Y*(Y^(1-p) - Y_hat^(1-p))/(1-p) - (Y^(2-p) - Y_hat^(2-p))/(2-p))
  }
  
  
  return(devs)
}