#####  normalise  #####
#' Normalise a vector
#' @description Normalise a vector by subtracting mean and dividing by standard deviation
#' @usage normalise(x)
#' @param x A numeric vector
#' @return A numeric vector
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' normalise(0:100)
#' @export

normalise <- function(x){
  (x-mean(x))/sd(x)
}