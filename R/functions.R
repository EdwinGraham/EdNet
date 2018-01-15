sigmoid <- function(x){
  p <- exp(x)/(1 + exp(x))
  p <- ifelse(is.na(p) & !is.na(x), 1, p)
  return(p)
}


relu <- function(x) pmax(x, 0)


initialiseParams <- function(layer_dims, activations, seed){
  L <- length(layer_dims)-1
  Params <- vector(mode="list", length=L)
  names(Params) <- paste0("l", seq(1, L))
  
  set.seed(seed)
  for(i in seq(1, L)){
    NCols <- layer_dims[i]
    NRows <- layer_dims[i+1]
    Weights <- matrix(rnorm(NCols*NRows, sd=sqrt(2/NCols)), ncol=NCols)
    Params[[paste0("l", i)]]$W <- Weights
    Params[[paste0("l", i)]]$b <- matrix(rep(0, NRows), nrow=NRows)
    Params[[paste0("l", i)]]$activation <- activations[[i]]
  }
  
  return(Params)
}


initialise_v <- function(Params){
  L <- length(Params)
  v <- vector(mode="list", length=L)
  names(v) <- paste0("l", seq(1, L))
  
  for(i in seq(1, L)){
    nrow <- nrow(Params[[paste0("l",i)]]$W)
    ncol <- ncol(Params[[paste0("l",i)]]$W)
    v[[paste0("l", i)]]$vdW <- matrix(nrow=nrow, ncol=ncol)
    v[[paste0("l", i)]]$vdb <- matrix(nrow=nrow, ncol=1)
    v[[paste0("l", i)]]$vdW[] <- rep(0, nrow*ncol)
    v[[paste0("l", i)]]$vdb[] <- rep(0, nrow)
  }
  
  return(v)
}


initialise_s <- function(Params){
  L <- length(Params)
  s <- vector(mode="list", length=L)
  names(s) <- paste0("l", seq(1, L))
  
  for(i in seq(1, L)){
    nrow <- nrow(Params[[paste0("l",i)]]$W)
    ncol <- ncol(Params[[paste0("l",i)]]$W)
    s[[paste0("l", i)]]$sdW <- matrix(nrow=nrow, ncol=ncol)
    s[[paste0("l", i)]]$sdb <- matrix(nrow=nrow, ncol=1)
    s[[paste0("l", i)]]$sdW[] <- rep(0, nrow*ncol)
    s[[paste0("l", i)]]$sdb[] <- rep(0, nrow)
  }
  
  return(s)
}


forwardPropagation <- function(X, Params, input_keep_prob, keep_prob){
  ## Number of layers
  L <- length(Params)
  
  ## Whether to apply dropout or not
  dropout <- keep_prob < 1
  
  ## Initialise Cache
  Cache <- vector(mode="list", length=L+1)
  names(Cache) <- paste0("l", seq(0, L))
  
  ## Input layer
  if(input_keep_prob < 1){
    Cache$l0$D <- matrix(nrow = dim(X)[1], ncol = dim(X)[2])
    Cache$l0$D[] <- 1*(runif(dim(X)[1]*dim(X)[2]) < keep_prob[1])
    Cache$l0$A <- X*Cache$l0$D
  } else{
    Cache$l0$A <- X
  }
  
  ## Other layers
  for(i in seq(1, L)){
    ## Get rows and columns for Z, A and D
    nrow <- nrow(Params[[paste0("l", i)]]$W)
    ncol <- ncol(Cache[[paste0("l", i-1)]]$A)
    ## Apply Dropout (add masks to cache if applied)
    if(dropout[i]){
      Cache[[paste0("l", i)]]$D <- matrix(nrow = nrow, ncol = ncol)
      Cache[[paste0("l", i)]]$D[] <- 1*(runif(nrow*ncol) < keep_prob[i])
    }
    ## Linear part
    Z <- matrix(nrow = nrow, ncol = ncol)
    Z[] <- apply(Params[[paste0("l", i)]]$W %*% Cache[[paste0("l", i-1)]]$A, 2, function(col) col + Params[[paste0("l", i)]]$b)
    ## Apply activation function (with dropout if needed)
    if(dropout[i]){
      Cache[[paste0("l", i)]]$A <- (do.call(Params[[paste0("l", i)]]$activation, list(Z)) * Cache[[paste0("l", i)]]$D) / keep_prob[i]
    } else{
      Cache[[paste0("l", i)]]$A <- do.call(Params[[paste0("l", i)]]$activation, list(Z))
    }
  }
  # Return cache
  return(Cache)
}


computeCost <- function(model, Y, lambda=0, alpha=0, family){
  ## Number of layers & Activations for final layer
  L <- length(model$Params)
  AL <- model$Cache[[paste0("l", L)]]$A
  
  ## Cost based on maximum likelihood
  J <- model$family$costfun(Y=Y, Yhat=AL)

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


backwardPropagation <- function(Y, model, alpha, lambda, keep_prob){
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
      dZ <- model$family$gradfun(Y=Y, Yhat=model$Cache[[paste0("l", i)]]$A)
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





updateParameters <- function(model, learning_rate, optimiser="GradientDescent", t=NULL, beta1=NULL, beta2=NULL, epsilon=NULL){
  L <- length(model$Params)
  
  # Initialise bias correction lists
  if(optimiser %in% c("Momentum", "Adam")){
    v_BiasCorrected <- vector(mode="list", length=length(model$v))
    names(v_BiasCorrected) <- names(model$v)
  }
  
  if(optimiser %in% c("RMSProp", "Adam")){
    s_BiasCorrected <- vector(mode="list", length=length(model$s))
    names(s_BiasCorrected) <- names(model$s)
  }
  
  for(i in seq(1, L)){
    # Update velocity
    if(optimiser %in% c("Momentum", "Adam")){
      model$v[[paste0("l", i)]]$vdW <- beta1 * model$v[[paste0("l", i)]]$vdW + (1 - beta1) * model$Grads[[paste0("l", i)]]$dW
      model$v[[paste0("l", i)]]$vdb <- beta1 * model$v[[paste0("l", i)]]$vdb + (1 - beta1) * model$Grads[[paste0("l", i)]]$db
      
      # Perform bias correction
      v_BiasCorrected[[paste0("l", i)]]$vdW <- model$v[[paste0("l", i)]]$vdW/(1 - beta1^t)
      v_BiasCorrected[[paste0("l", i)]]$vdb <- model$v[[paste0("l", i)]]$vdb/(1 - beta1^t)
    }
    
    # Update moving average of squared gradients
    if(optimiser %in% c("RMSProp", "Adam")){
      model$s[[paste0("l", i)]]$sdW <- beta2 * model$s[[paste0("l", i)]]$sdW + (1 - beta2) * model$Grads[[paste0("l", i)]]$dW^2
      model$s[[paste0("l", i)]]$sdb <- beta2 * model$s[[paste0("l", i)]]$sdb + (1 - beta2) * model$Grads[[paste0("l", i)]]$db^2
      
      # Perform bias correction
      s_BiasCorrected[[paste0("l", i)]]$sdW <- model$s[[paste0("l", i)]]$sdW/(1 - beta2^t)
      s_BiasCorrected[[paste0("l", i)]]$sdb <- model$s[[paste0("l", i)]]$sdb/(1 - beta2^t)
    }
    
    # Update Parameters
    if(optimiser == c("GradientDescent")){
      model$Params[[paste0("l", i)]]$W <- model$Params[[paste0("l", i)]]$W - learning_rate * model$Grads[[paste0("l", i)]]$dW
      model$Params[[paste0("l", i)]]$b <- model$Params[[paste0("l", i)]]$b - learning_rate * model$Grads[[paste0("l", i)]]$db
    } else if(optimiser == c("Momentum")){
      model$Params[[paste0("l", i)]]$W <- model$Params[[paste0("l", i)]]$W - learning_rate * v_BiasCorrected[[paste0("l", i)]]$vdW
      model$Params[[paste0("l", i)]]$b <- model$Params[[paste0("l", i)]]$b - learning_rate * v_BiasCorrected[[paste0("l", i)]]$vdb
    } else if(optimiser == c("RMSProp")){
      model$Params[[paste0("l", i)]]$W <- model$Params[[paste0("l", i)]]$W - learning_rate * model$Grads[[paste0("l", i)]]$dW / (sqrt(s_BiasCorrected[[paste0("l", i)]]$sdW) + epsilon)
      model$Params[[paste0("l", i)]]$b <- model$Params[[paste0("l", i)]]$b - learning_rate * model$Grads[[paste0("l", i)]]$db / (sqrt(s_BiasCorrected[[paste0("l", i)]]$sdb) + epsilon)
    } else if(optimiser == c("Adam")){
      model$Params[[paste0("l", i)]]$W <- model$Params[[paste0("l", i)]]$W - learning_rate * v_BiasCorrected[[paste0("l", i)]]$vdW / (sqrt(s_BiasCorrected[[paste0("l", i)]]$sdW) + epsilon)
      model$Params[[paste0("l", i)]]$b <- model$Params[[paste0("l", i)]]$b - learning_rate * v_BiasCorrected[[paste0("l", i)]]$vdb / (sqrt(s_BiasCorrected[[paste0("l", i)]]$sdb) + epsilon)
    } else stop("Only GradientDescent, Momentum, RMSProp and Adam supported.")
  }
  return(model)
}