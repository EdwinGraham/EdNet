#####  predict.EdNetModel  #####
#' Predict for EdNetModel objects
#' @description Predict for EdNetModel objects
#' @usage predict.EdNetModel(object, newdata=NULL)
#' @param object An object of class EdNetModel
#' @param newdata A matrix with rows as training examples and columns as input features, with the same width as the data used to train object.
#' @return A vector or matrix of predictions.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # No example yet
#' @export

predict.EdNetModel <- function(object, newdata=NULL){
  ## Check data
  if(is.null(newdata)){
    if(is.null(object@data$X)) stop("No data to predict on.")
    newdata <- object@data$X
  } else newdata <- t(newdata)
  
  ## Run forward prop
  L <- length(object@model$Params)
  cache <- forwardPropagation(newdata, object@model$Params, 1, rep(1, L))
  
  ## Return activations for final layer
  return(t(cache[[paste0("l", L)]]$A))
}