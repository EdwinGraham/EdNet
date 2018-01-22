costCategorical <- function(Y, Y_hat, weight=NULL){
  if(is.null(weight)){
    mean(devianceCategorical(Y, Y_hat))
  } else{
    weightedMean(devianceCategorical(Y, Y_hat), weight)
  }
}