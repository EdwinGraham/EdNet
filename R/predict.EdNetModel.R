predict.EdNetModel <- function(object, newdata=NULL){
  ## Check data
  if(is.null(newdata)){
    if(is.null(object@data$X)) stop("No data to predict on.")
    newdata <- object@data$X
  }
  
  ## Run forward prop
  L <- length(object@model$Params)
  cache <- forwardPropagation(newdata, object@model$Params, 1, rep(1, L))
  
  ## Return activations for final layer
  return(cache[[paste0("l", L)]]$A)
}