generateMiniBatches <- function(X, Y, m, mini_batch_size, weight=NULL, offset=NULL){
  permutate <- sample(seq(1, m))
  X <- X[, permutate, drop=FALSE]
  Y <- Y[, permutate, drop=FALSE]
  if(!is.null(weight)) weight <- weight[permutate]
  if(!is.null(offset)) offset <- offset[, permutate, drop=FALSE]

  return(lapply(seq(0, ceiling(m / mini_batch_size)-1), function(i){
    returnList <- list(X=X[, seq(mini_batch_size*i+1, min(mini_batch_size*(i+1), m)), drop=FALSE],
                       Y=Y[, seq(mini_batch_size*i+1, min(mini_batch_size*(i+1), m)), drop=FALSE])
    if(!is.null(weight)){
      returnList$weight <- weight[seq(mini_batch_size*i+1, min(mini_batch_size*(i+1), m))]
    } else{
      returnList$weight <- NULL
    }
    if(!is.null(offset)){
      returnList$offset <- offset[, seq(mini_batch_size*i+1, min(mini_batch_size*(i+1), m)), drop=FALSE]
    } else{
      returnList$offset <- NULL
    }
    return(returnList)
  }))
}

