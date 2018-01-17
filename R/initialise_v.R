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