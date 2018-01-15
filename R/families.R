outputFamily <- function(family, power=NULL){
  if(family=="binary"){
    link.inv <- sigmoid
    costfun <- costfun.binary
    gradfun <- function(Y, Yhat) Yhat - Y
  } else if(family=="multiclass"){
    link.inv <- softmax
    costfun <- costfun.binary
    gradfun <- function(Y, Yhat) Yhat - Y
  } else if(family=="gaussian"){
    link.inv <- identity
    costfun <- costfun.gaussian
    gradfun <- function(Y, Yhat) Yhat - Y
  } else if(family=="poisson"){
    link.inv <- exp
    costfun <- costfun.poisson
    gradfun <- function(Y, Yhat) Yhat - Y
  } else if(family=="gamma"){
    link.inv <- exp
    costfun <- costfun.gamma
    gradfun <- function(Y, Yhat) 1 - Y/Yhat
  } else if(family=="tweedie"){
    link.inv <- exp
    stop("tweedie still unsupported")
    costfun <- 1
    gradfun <- 1
  } else stop("family should be one of binary, multiclass, gaussian, poisson, gamma, tweedie.")

  f <- list(family=family, link.inv=link.inv, costfun=costfun, gradfun=gradfun)
  if(family=="tweedie") f$power <- power
  return(f)
}

softmax <- function(x){
  x <- exp(x)
  x <- t(t(x)/colSums(x))
  return(x)
}

costfun.binary <- function(Y, Yhat){
  eps <- 1e-15
  Yhat <- pmax(pmin(Yhat, 1 - eps), eps)
  J <- -mean(Y*log(Yhat) + (1-Y)*log(1-Yhat))
  return(J)
}

costfun.gaussian <- function(Y, Yhat){
  J <- (1/2)*mean((Y-Yhat)^2)
  return(J)
}

costfun.poisson <- function(Y, Yhat){
  eps <- 1e-15
  Yhat <- pmax(Yhat, eps)
  J <- mean(Yhat-Y*log(Yhat))
  return(J)
}

costfun.gamma <- function(Y, Yhat){
  eps <- 1e-15
  Yhat <- pmax(Yhat, eps)
  J <- mean(Y/Yhat-log(Yhat))
  return(J)
}