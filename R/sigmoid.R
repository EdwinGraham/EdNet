#####  sigmoid  #####
#' Compute sigmoid function on vector
#' @description Compute sigmoid function on vector
#' @usage sigmoid(x)
#' @param x A numeric vector
#' @return A numeric vector the same length as x.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # No example yet
#' @export

sigmoid <- function(x){
  p <- exp(x)/(1 + exp(x))
  p <- ifelse(is.na(p) & !is.na(x), 1, p)
  return(p)
}