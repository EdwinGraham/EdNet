initialiseParams <- function(layer_dims, activations, seed, initialisation_constant=2){
  L <- length(layer_dims)-1
  Params <- vector(mode="list", length=L)
  names(Params) <- paste0("l", seq(1, L))
  
  set.seed(seed)
  for(i in seq(1, L)){
    NCols <- layer_dims[i]
    NRows <- layer_dims[i+1]
    Weights <- matrix(rnorm(NCols*NRows, sd=sqrt(initialisation_constant/NCols)), ncol=NCols)
    Params[[paste0("l", i)]]$W <- Weights
    Params[[paste0("l", i)]]$b <- matrix(rep(0, NRows), nrow=NRows)
    Params[[paste0("l", i)]]$activation <- activations[[i]]
  }
  
  return(Params)
}