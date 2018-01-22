costTweedie <- function(Y, Y_hat, p, weight=NULL){
  if(is.null(weight)){
    mean(devianceTweedie(Y, Y_hat, p))
  } else{
    weightedMean(devianceTweedie(Y, Y_hat, p), weight)
  }
}