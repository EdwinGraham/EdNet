computeCost <- function(model, Y, lambda=0, alpha=0, family){
  ## Number of layers & Activations for final layer
  L <- length(model$Params)
  AL <- model$Cache[[paste0("l", L)]]$A
  
  ## Cost based on maximum likelihood
  J <- model$family$costfun(Y=Y, Y_hat=AL)
  
  if(lambda>0){
    m <- dim(Y)[2]
    L2_regularization_cost <- (1 / m) * (lambda/2) * sum(sapply(seq(1, L), function(i) sum(model$Params[[paste0("l", i)]]$W^2)))
    J <- J + L2_regularization_cost
  }
  
  if(alpha>0){
    m <- dim(Y)[2]
    L1_regularization_cost <- (1 / m) * alpha * sum(sapply(seq(1, L), function(i) sum(abs(model$Params[[paste0("l", i)]]$W))))
    J <- J + L1_regularization_cost
  }
  
  return(J)
}