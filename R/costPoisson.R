costPoisson <- function(Y, Y_hat, weight=NULL){
  if(is.null(weight)){
    mean(deviancePoisson(Y, Y_hat))
  } else{
    weightedMean(deviancePoisson(Y, Y_hat), weight)
  }
}