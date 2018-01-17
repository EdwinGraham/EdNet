sigmoid <- function(x){
  p <- exp(x)/(1 + exp(x))
  p <- ifelse(is.na(p) & !is.na(x), 1, p)
  return(p)
}