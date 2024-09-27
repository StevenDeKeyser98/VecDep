#' @title covgpenal
#'
#' @description This function computes the empirical penalized Gaussian copula covariance matrix with the Gaussian log-likelihood
#' plus a lasso-type penalty as objective function.
#' Model selection is done by choosing omega such that BIC is maximal.
#'
#' @param S The sample matrix of normal scores covariances.
#' @param n The sample size.
#' @param omegas The candidate values for the tuning parameter in \eqn{[0,\infty)}.
#' @param derpenal The derivative of the penalty function to be used (default = scad with parameter a = 3.7).
#' @param nsteps The number of weighted covariance graphical lasso iterations (default = 1).
#'
#' @details
#' The aim is to solve/compute \deqn{\widehat{\boldsymbol{\Sigma}}_{\text{LT},n} = \text{arg min}_{\boldsymbol{\Sigma} > 0} \left \{
#' \ln \left | \boldsymbol{\Sigma} \right | + \text{tr} \left (\boldsymbol{\Sigma}^{-1} \widehat{\boldsymbol{\Sigma}}_{n} \right ) + P_{\text{LT}}\left (\boldsymbol{\Sigma},\omega_{n} \right ) \right \},}
#' where the penalty function \eqn{P_{\text{LT}}} is of lasso-type:
#' \deqn{P_{\text{LT}} \left (\boldsymbol{\Sigma},\omega_{n} \right ) = \sum_{ij} p_{\omega_{n}} \left (\Delta_{ij} \left |\sigma_{ij} \right | \right ),}
#' for a certain penalty function \eqn{p_{\omega_{n}}} with penalty parameter \eqn{\omega_{n}}, and \eqn{\sigma_{ij}} the
#' \eqn{(i,j)}'th entry of \eqn{\boldsymbol{\Sigma}} with \eqn{\Delta_{ij} = 1} if \eqn{i \neq j} and \eqn{\Delta_{ij} = 0} if \eqn{i = j} (in order to not shrink the variances).
#' The matrix \eqn{\widehat{\boldsymbol{\Sigma}}_{n}} is the matrix of sample normal scores covariances.
#'
#' In case \eqn{p_{\omega_{n}}(t) = \omega_{n} t} is the lasso penalty, the implementation for the
#' (weighted) covariance graphical lasso is available in the R package 'covglasso'. For general penalty functions,
#' we perform a local linear approximation to the penalty function and do (nsteps, default = 1) weighted covariance graphical lasso optimizations.
#'
#' The default for the penalty function is the scad (derpenal = derivative of scad penalty), i.e.,
#' \deqn{p_{\omega_{n},\text{scad}}^{\prime}(t) = \omega_{n} \left [1 \left (t \leq \omega_{n} \right ) + \frac{\max \left (a \omega_{n} - t,0 \right )}{\omega_{n} (a-1)} 1 \left (t > \omega_{n} \right ) \right ],}
#' with \eqn{a = 3.7} by default.
#'
#' For tuning \eqn{\omega_{n}}, we maximize (over a grid of candidate values) the BIC criterion
#' \deqn{\text{BIC} \left (\widehat{\boldsymbol{\Sigma}}_{\omega_{n}} \right ) = -n \left [\ln \left |\widehat{\boldsymbol{\Sigma}}_{\omega_{n}} \right | + \text{tr} \left (\widehat{\boldsymbol{\Sigma}}_{\omega_{n}}^{-1} \widehat{\boldsymbol{\Sigma}}_{n} \right ) \right ] - \ln(n) \text{df} \left (\widehat{\boldsymbol{\Sigma}}_{\omega_{n}} \right ),}
#' where \eqn{\widehat{\boldsymbol{\Sigma}}_{\omega_{n}}} is the estimated candidate covariance matrix using \eqn{\omega_{n}}
#' and df (degrees of freedom) equals the number of non-zero entries in \eqn{\widehat{\boldsymbol{\Sigma}}_{\omega_{n}}}, not taking the elements under the diagonal into account.
#'
#' @return A list with elements "est" containing the (lasso-type) penalized matrix of sample normal scores rank correlations (output as provided by the function covglasso.R), and "omega" containing the optimal tuning parameter.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' High-dimensional copula-based Wasserstein dependence.
#' doi: https://doi.org/10.48550/arXiv.2404.07141.
#'
#' Fop, M. (2021).
#' covglasso: Sparse Covariance Matrix Estimation, R package version 1.0.3.
#' url: https://CRAN.R-project.org/package=covglasso.
#'
#' Wang, H. (2014).
#' Coordinate descent algorithm for covariance graphical lasso.
#' Statistics and Computing 24:521-529.
#' doi: https://doi.org/10.1007/s11222-013-9385-5.
#'
#' @examples
#' q = 10
#' dim = c(5,5)
#' n = 100

#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' R0 = createR0(R,dim) # Sparsity on off-diagonal blocks
#' sample = mvtnorm::rmvnorm(n,rep(0,q),R0,method = "chol") # Sample from multivariate normal distribution
#' scores = matrix(0,n,q) # Normal scores
#' for(j in 1:q){scores[,j] = qnorm((n/(n+1)) * ecdf(sample[,j])(sample[,j]))}
#' Sigma_est = cov(scores) * ((n-1)/n) # Sample matrix of normal scores covariances
#' omega = seq(0.01, 0.6, length = 50) # Candidate tuning parameters
#' Sigma_est_penal = covgpenal(Sigma_est,n,omega)
#'
#' @export


covgpenal = function(S, n, omegas, derpenal = function(t,omega){derSCAD(t,omega,3.7)}, nsteps = 1){

  q = nrow(S) # Total dimension

  delta = matrix(1,q,q) ; diag(delta) = 0 # Do not penalize diagonal elements (variances)
  bic = integer(length(omegas)) # We want to maximize BIC
  ests = list() # Estimates

  # Weighted covariance graphical lasso

  for(o in 1:length(omegas)){ # For each candidate omega

    omega = omegas[o]
    Sigma0 = S # Starting value

    for(i in 1:nsteps){ # Typically 1 step

      deltaT = delta * apply(delta * abs(Sigma0),1:2,function(t){derpenal(t,omega)}) # Delta tilde matrix
      array = array(deltaT, dim = c(q,q,1))
      covgl = covglasso::covglasso(S = S, n = n, lambda = array, start = S) # Solve weighted graphical lasso
      Sigma = covgl$sigma # Sigma estimate
      Sigma0 = Sigma # New starting value

    }

    bic[o] = as.numeric(covgl$BIC) # BIC values
    ests[[o]] = covgl

  }

  best = which.max(bic) # Best BIC value

  return(list("est" = ests[[best]], "omega" = omegas[best])) # Return best estimate and corresponding omega parameter

}


derSCAD = function(t,omega,a){

  # Derivative of SCAD penalty

  omega * ((t <= omega) + (max(a*omega - t,0)/((a-1) * omega)) * (t > omega))

}
