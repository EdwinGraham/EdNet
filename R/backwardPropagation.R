backwardPropagation <- function(Y, model, alpha, lambda, keep_prob, weight=NULL){
  ## Helpful parameters
  m <- dim(Y)[2]
  L <- length(model$Params)
  
  ## Initialise Grads
  Grads <- vector(mode="list", length = L)
  names(Grads) <- c(paste0("l", seq(1, L)))
  
  ## Apply dropout or not
  dropout <- keep_prob < 1
  
  ## Run through layers in reverse order
  for(i in seq(L, 1)){
    ## dZ for Output layer
    if(i == L){
      dZ <- model$family$gradfun(Y=Y, Y_hat=model$Cache[[paste0("l", i)]]$A, weight)
      ## dZ for hidden layers
    } else{
      if(dropout[i]){
        dA <- (t(model$Params[[paste0("l", i+1)]]$W) %*% dZ_prev) * model$Cache[[paste0("l", i)]]$D / keep_prob[i]
      } else{
        dA <- t(model$Params[[paste0("l", i+1)]]$W) %*% dZ_prev
      }
      if(model$Params[[paste0("l", i)]]$activation =="relu"){
        dZ <- dA * (model$Cache[[paste0("l", i)]]$A > 0)
      } else if(model$Params[[paste0("l", i)]]$activation=="tanh"){
        dZ <- dA * (1 - tanh(model$Cache[[paste0("l", i)]]$A)^2)
      } else stop("Only relu and tanh activations supported for hidden layers.")
    }
    ## Calculate dW and db for layer and add them to Grads
    Grads[[paste0("l", i)]]$dW <- (1 / m) * dZ %*% t(model$Cache[[paste0("l", i-1)]]$A) + (lambda/m)*model$Params[[paste0("l", i)]]$W + (alpha/m)*sign(model$Params[[paste0("l", i)]]$W)
    Grads[[paste0("l", i)]]$db <- matrix(nrow=nrow(model$Params[[paste0("l", i)]]$b), ncol=1)
    Grads[[paste0("l", i)]]$db[] <- (1 / m) * rowSums(dZ)
    
    ## Carry dZ into next loop as dZ_prev
    dZ_prev <- dZ
  }
  
  ## Return Grads
  return(Grads)
}