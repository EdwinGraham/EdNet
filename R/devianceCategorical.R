#####  devianceCategorical  #####
#' Function to calculate deviance for model predictions assuming a Categorical distribution.
#' @description This function calculates a deviance measure for model predictions assuming a Categorical distribution.
#' @usage devianceCategorical(y, y_hat)
#' @param y a numeric matrix of observations (0s and 1s) where columns indicate categories and rows indicate observations. Should be exactly one 1 per row.
#' @param y_hat a numeric matrix of predictions (between 0 and 1) for y (must have same dimensions as y.) Row sums should all equal 1.
#' @return a numeric vector.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # n <- 1000
#' # nCat <- 10
#' # 
#' # # Random probabilities normalised by row
#' # true_logistic <- matrix(rnorm(n*nCat), ncol = nCat)
#' # true_probabilities <- exp(true_logistic)/rowSums(exp(true_logistic))
#' # 
#' # # Generate observations
#' # observed <- t(apply(true_probabilities, 1, cumsum))
#' # observed <- 1*(observed > runif(n))
#' # observed <- 1*t(apply(observed, 1, cumsum)==1)
#' # 
#' # # Generate predictions
#' # predicted <- true_logistic + matrix(rnorm(n*nCat, sd=0.1), ncol=nCat)
#' # predicted <- exp(predicted)/rowSums(exp(predicted))
#' # 
#' # plot(observed, predicted)
#' # 
#' # devs <- devianceCategorical(observed, predicted)
#' # sum(devs)
#' @export

devianceCategorical <- function(y, y_hat){
  if(!is.matrix(y)) stop("y should be a matrix")
  if(!is.matrix(y_hat)) stop("y_hat should be a matrix")
  
  if(!identical(dim(y), dim(y_hat))) stop("y and y_hat must have the same dimensions")
  
  # Fix for very small values
  eps <- 1E-16
  y_hat <- pmin(pmax(y_hat, eps), 1-eps)
  
  devs <- -2*Matrix::rowSums(y*log(y_hat))

  return(devs)
}