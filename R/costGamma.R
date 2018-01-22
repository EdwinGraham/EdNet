costGamma <- function(Y, Y_hat, weight=NULL){
  if(is.null(weight)){
    mean(devianceGamma(Y, Y_hat))
  } else{
    weightedMean(devianceGamma(Y, Y_hat), weight)
  }
}