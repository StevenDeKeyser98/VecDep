library(testthat)
library(VecDep)

# Tests for the function betakernelestimator.R

test_that("betakernelestimator() returns a number", {

  q = 3
  n = 100

  sample = mvtnorm::rmvnorm(n,rep(0,q),diag(3),method = "chol") # Sample from multivariate normal distribution with identity covariance matrix
  pseudos = matrix(0,n,q)
  for(j in 1:q){pseudos[,j] = (n/(n+1)) * ecdf(sample[,j])(sample[,j])} # Copula pseudo-observations

  input = rep(0.5,q) # Argument at which to estimate the density
  h = hamse(input,pseudos = pseudos,n = n,estimator = "beta",bw_method = 1)
  output <- betakernelestimator(input = input, h = h, pseudos = pseudos)
  expect_type(output, "double")

})





