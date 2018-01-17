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