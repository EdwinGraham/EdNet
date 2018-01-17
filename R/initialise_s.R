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