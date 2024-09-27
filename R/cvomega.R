#' @title cvomega
#'
#' @description This functions selects the omega tuning parameter for ridge penalization of the empirical Gaussian copula correlation matrix via cross-validation.
#'              The objective function is the Gaussian log-likelihood, and a grid search is performed using K folds.
#'
#' @param sample  A sample from a q-dimensional random vector \eqn{\mathbf{X}} (n x q matrix with observations in rows, variables in columns).
#' @param omegas  A grid of candidate penalty parameters in \eqn{[0,1]}.
#' @param K       The number of folds to be used.
#'
#' @details  The loss function is the Gaussian log-likelihood, i.e., given an estimated (penalized)
#'           Gaussian copula correlation matrix (normal scores rank correlation matrix) \eqn{\widehat{\mathbf{R}}_{n}^{(-j)}} computed on a training set leaving out fold j, and
#'           \eqn{\widehat{\mathbf{R}}_{n}^{(j)}} the empirical (non-penalized)
#'           Gaussian copula correlation matrix computed on test fold j, we search for the tuning parameter that minimizes
#'           \deqn{\sum_{j = 1}^{K} \left [\ln \left ( \left | \widehat{\mathbf{R}}_{n}^{(-j)} \right | \right ) + \text{tr} \left \{\widehat{\mathbf{R}}_{n}^{(j)} \left (\widehat{\mathbf{R}}_{n}^{(-j)} \right )^{-1} \right \} \right ].}
#'           The underlying assumption is that the copula of \eqn{\mathbf{X}} is Gaussian.
#'
#' @return The optimal ridge penalty parameter minimizing the cross-validation error.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' High-dimensional copula-based Wasserstein dependence.
#' doi: https://doi.org/10.48550/arXiv.2404.07141.
#'
#' Warton, D.I. (2008).
#' Penalized normal likelihood and ridge regularization of correlation and covariance matrices.
#' Journal of the American Statistical Association 103(481):340-349.
#' doi: https://doi.org/10.1198/016214508000000021.
#'
#' @seealso \code{\link{estR}} for computing the (Ridge penalized) empirical Gaussian copula correlation matrix.
#'
#'
#' @examples
#' q = 10
#' n = 50

#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' sample = mvtnorm::rmvnorm(n,rep(0,q),R,method = "chol") # Sample from multivariate normal distribution
#' omega = cvomega(sample = sample,omegas = seq(0.01,0.999,len = 50),K = 5) # 5-fold cross-validation with Gaussian likelihood as loss for selecting omega
#' R_est = estR(sample,omega = omega)

#' @export

cvomega = function(sample, omegas, K){

  n = nrow(sample) # Sample size
  q = ncol(sample) # Total dimension

  scores = matrix(0,n,q) # Matrix for normal scores

  for(j in 1:q){

    scores[,j] = qnorm((n/(n+1)) * ecdf(sample[,j])(sample[,j])) # Normal scores

  }

  LLs = integer(length(omegas))

  for(l in 1:length(omegas)){

    LLs[l] = CVLF(omegas[l],scores,K) # Objectives for each candidate omega

  }

  return(omegas[which.max(LLs)]) # Omega that maximizes the objective

}

# Auxiliary functions


LogL = function(data,R,omega){

  # Log-likelihood of Gaussian model with correlation matrix R and penalty parameter omega.
  # data is the validation data, R is the normal scores rank correlation matrix based on training data, omega is the penalty parameter.

  q = nrow(R)
  n = nrow(data)
  sigmaD = diag(rep(sqrt(sum(qnorm(seq(1,n)/(n+1))^2)/(n-1)),q)) # Diagonal matrix with normal scores standard deviations (independent of the data)
  sigmaL = (1/omega) * sigmaD %*% (omega * R + (1-omega) * diag(q)) %*% sigmaD # Penalized estimate based on training data
  t1 = n * q * log(2*pi) + n * log(det(sigmaL)) # First term for log-likelihood
  t2 = sum(diag(data %*% solve(sigmaL) %*% t(data))) # Second term for log-likelihood

  return((-1/2)*(t1 + t2))

}

CVLF = function(omega,data,K){

  # Cross-validated log-likelihood function with penalty parameter omega using K folds.
  # data should be normal scores.

  n = nrow(data)
  ind = sample(seq(1,n)) # Indices for folds
  fld_size = floor(n/K) # Size of each fold
  flds = list() # Folds

  for(i in 1:K){

    flds[[i]] = ind[((fld_size)*(i-1) + 1):(fld_size*i)] # Assign indices to each fold

  }

  if(n-(K*fld_size) > 0){ # If n is not a multiple of K

    for(i in 1:(n-(K*fld_size))){

      flds[[i]] = c(flds[[i]],ind[fld_size * K + i]) # Assign remaining indices to first folds

    }
  }

  LL = 0 # Sum of log-likelihood objective over all folds

  for(i in 1:K){

    valid = data[flds[[i]],] # Validation data
    train = data[setdiff(seq(1,n),flds[[i]]),] # Training data
    R_est = cor(train) # Normal scores rank correlation matrix computed using training data
    LL = LL + LogL(valid,R_est,omega)

  }

  return(LL)

}
