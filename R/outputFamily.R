outputFamily <- function(family, power=NULL){
  if(family=="binary"){
    link.inv <- sigmoid
    costfun <- function(Y, Y_hat) mean(devianceBernoulli(Y, Y_hat))
    gradfun <- function(Y, Y_hat) Y_hat - Y
  } else if(family=="multiclass"){
    link.inv <- softmax
    costfun <- function(Y, Y_hat) mean(devianceCategorical(Y, Y_hat))
    gradfun <- function(Y, Y_hat) Y_hat - Y
  } else if(family=="gaussian"){
    link.inv <- identity
    costfun <- function(Y, Y_hat) mean(devianceNormal(Y, Y_hat))
    gradfun <- function(Y, Y_hat) Y_hat - Y
  } else if(family=="poisson"){
    link.inv <- exp
    costfun <- function(Y, Y_hat) mean(deviancePoisson(Y, Y_hat))
    gradfun <- function(Y, Y_hat) Y_hat - Y
  } else if(family=="gamma"){
    link.inv <- exp
    costfun <- function(Y, Y_hat) mean(devianceGamma(Y, Y_hat))
    gradfun <- function(Y, Y_hat) 1 - Y/Y_hat
  } else if(family=="tweedie"){
    if(is.null(power)) stop("tweedie power parameter unspecified")
    link.inv <- exp
    costfun <- function(Y, Y_hat) mean(devianceTweedie(Y, Y_hat, p=power))
    gradfun <- function(Y, Y_hat) (Y_hat^(1-power))*(Y_hat - Y)
  } else stop("family should be one of binary, multiclass, gaussian, poisson, gamma, tweedie.")

  f <- list(family=family, link.inv=link.inv, costfun=costfun, gradfun=gradfun)
  if(family=="tweedie") f$power <- power
  return(f)
}
