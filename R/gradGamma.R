gradGamma <- function(Y, Y_hat, weight=NULL){
  # setup return vector
  grads <- matrix(nrow = nrow(Y), ncol=ncol(Y))
  
  # Gradient is zero when Y=Y_hat
  exact <- Y==Y_hat
  grads[exact] <- 0
  
  # Fix for very small values
  eps <- 1E-16
  Y_hat <- pmax(Y_hat, eps)
  
  # Fix for very small values
  grads[!exact] <- 1 - Y[!exact]/Y_hat[!exact]
  
  # Weights
  if(!is.null(weight)) grads <- grads * matrix(weight, nrow=1)

  return(grads)
}