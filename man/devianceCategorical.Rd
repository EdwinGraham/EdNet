% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/devianceCategorical.R
\name{devianceCategorical}
\alias{devianceCategorical}
\title{Function to calculate deviance for model predictions assuming a Categorical distribution.}
\usage{
devianceCategorical(Y, Y_hat)
}
\arguments{
\item{Y}{a numeric matrix of observations (0s and 1s) where columns indicate categories and rows indicate observations. Should be exactly one 1 per row.}

\item{Y_hat}{a numeric matrix of predictions (between 0 and 1) for Y (must have same dimensions as Y.) Row sums should all equal 1.}
}
\value{
a numeric vector.
}
\description{
This function calculates a deviance measure for model predictions assuming a Categorical distribution.
}
\examples{
# n <- 1000
# nCat <- 10
# 
# # Random probabilities normalised by row
# true_logistic <- matrix(rnorm(n*nCat), ncol = nCat)
# true_probabilities <- exp(true_logistic)/rowSums(exp(true_logistic))
# 
# # Generate observations
# observed <- t(apply(true_probabilities, 1, cumsum))
# observed <- 1*(observed > runif(n))
# observed <- 1*t(apply(observed, 1, cumsum)==1)
# 
# # Generate predictions
# predicted <- true_logistic + matrix(rnorm(n*nCat, sd=0.1), ncol=nCat)
# predicted <- exp(predicted)/rowSums(exp(predicted))
# 
# plot(observed, predicted)
# 
# devs <- devianceCategorical(observed, predicted)
# sum(devs)
}
\author{
Edwin Graham <edwingraham1984@gmail.com>
}
