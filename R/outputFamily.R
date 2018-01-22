outputFamily <- function(family, power=NULL){
  if(family=="binary"){
    link.inv <- sigmoid
    costfun <- costBernoulli
    gradfun <- gradGeneric
  } else if(family=="multiclass"){
    link.inv <- softmax
    costfun <- costCategorical
    gradfun <- gradGeneric
  } else if(family=="gaussian"){
    link.inv <- identity
    costfun <- costNormal
    gradfun <- gradGeneric
  } else if(family=="poisson"){
    link.inv <- exp
    costfun <- costPoisson
    gradfun <- gradGeneric
  } else if(family=="gamma"){
    link.inv <- exp
    costfun <- costGamma
    gradfun <- gradGamma
  } else if(family=="tweedie"){
    if(is.null(power)) stop("tweedie power parameter unspecified")
    link.inv <- exp
    costfun <- function(Y, Y_hat, weight=NULL) costTweedie(Y, Y_hat, p=power, weight)
    gradfun <- function(Y, Y_hat, weight=NULL) gradTweedie(Y, Y_hat, p=power, weight)
  } else stop("family should be one of binary, multiclass, gaussian, poisson, gamma, tweedie.")

  f <- list(family=family, link.inv=link.inv, costfun=costfun, gradfun=gradfun)
  if(family=="tweedie") f$power <- power
  return(f)
}
