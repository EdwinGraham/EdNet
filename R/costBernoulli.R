costBernoulli <- function(Y, Y_hat, weight=NULL){
  if(is.null(weight)){
    mean(devianceBernoulli(Y, Y_hat))
  } else{
    weightedMean(devianceBernoulli(Y, Y_hat), weight)
  }
}