#####  normalise  #####
#' One-hot encode a factor
#' @description One-hot encode a factor
#' @usage onehotEncode(x)
#' @param x A factor
#' @return A matrix of ones and zeros with length(x) rows and length(levels(x)) columns. If there is only one level returns a vector.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' normalise(0:100)
#' @export

onehotEncode <- function(x){
  if(!is.factor(x)) stop("x must be a factor")
  return(sapply(levels(x), function(l) 1*(x==l)))
}