gradGeneric <- function(Y, Y_hat, weight=NULL){
  if(is.null(weight)){
    Y_hat - Y
  } else{
    weight * (Y_hat - Y)
  }
}