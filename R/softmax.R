softmax <- function(x){
  x <- exp(x)
  x <- t(t(x)/colSums(x))
  return(x)
}