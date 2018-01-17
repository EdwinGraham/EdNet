#####  devianceNormal  #####
#' Function to calculate deviance for model predictions assuming a Normal distribution.
#' @description This function calculates a deviance measure for model predictions assuming a Normal distribution.
#' @usage devianceNormal(Y, Y_hat)
#' @param Y a numeric vector of observations.
#' @param Y_hat a numeric vector of predictions for Y (must have same length as Y.)
#' @return a numeric vector.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # n <- 1000
#' # 
#' # true <- rnorm(n)
#' # observed <- rnorm(n, true, sd=0.1)
#' # predicted <- true/2 + rnorm(n, sd=0.2)
#' # 
#' # plot(observed, predicted)
#' # 
#' # devs <- devianceNormal(observed, predicted)
#' # sum(devs)
#' @export

devianceNormal <- function(Y, Y_hat, w=NULL){
  n <- length(Y)
  if(length(Y_hat) != n) stop("Y and Y_hat are not the same length")

  devs <- (Y-Y_hat)^2
  
  return(devs)
}


