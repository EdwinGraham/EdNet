% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/devianceTweedie.R
\name{devianceTweedie}
\alias{devianceTweedie}
\title{Function to calculate deviance for model predictions assuming a Tweedie distribution.}
\usage{
devianceTweedie(Y, Y_hat, p=1.5)
}
\arguments{
\item{Y}{a numeric vector of observations.}

\item{Y_hat}{a numeric vector of predictions for Y (must have same length as Y.)}

\item{p}{Tweedie distribution variance power parameter}
}
\value{
a numeric vector.
}
\description{
This function calculates a deviance measure for model predictions assuming a Tweedie distribution.
}
\examples{
# n <- 1000
# 
# true <- rgamma(n, shape=15, scale=10)
# observed <- rtweedie(n, true, phi=50, p=1.5)
# predicted <- exp(log(150)/3 + 2*log(true)/3 + rnorm(n, sd=0.1))
# 
# plot(observed, predicted)
# 
# devs <- devianceTweedie(observed, predicted)
# sum(devs)
}
\author{
Edwin Graham <edwingraham1984@gmail.com>
}
