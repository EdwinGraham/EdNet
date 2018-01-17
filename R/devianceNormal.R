#####  devianceNormal  #####
#' Function to calculate deviance for model predictions assuming a Normal distribution.
#' @description This function calculates a deviance measure for model predictions assuming a Normal distribution.
#' @usage devianceNormal(y, y_hat)
#' @param y a numeric vector of observations.
#' @param y_hat a numeric vector of predictions for y (must have same length as y.)
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

devianceNormal <- function(y, y_hat, w=NULL){
  n <- length(y)
  if(length(y_hat) != n) stop("y and y_hat are not the same length")

  devs <- (y-y_hat)^2
  
  return(devs)
}


