onehotEncode <- function(x){
  if(!is.factor(x)) stop("x must be a factor")
  return(sapply(levels(x), function(l) 1*(x==l)))
}