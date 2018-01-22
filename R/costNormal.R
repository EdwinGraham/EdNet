costNormal <- function(Y, Y_hat, weight=NULL){
  if(is.null(weight)){
    mean(devianceNormal(Y, Y_hat))
  } else{
    weightedMean(devianceNormal(Y, Y_hat), weight)
  }
}