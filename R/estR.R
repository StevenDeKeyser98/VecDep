#' @title estR
#'
#' @description This function computes the sample Q-scores rank correlation matrix.
#' A ridge penalization is possible.
#'
#' @param sample A sample from a q-dimensional random vector \eqn{\mathbf{X}} (n x q matrix with observations in rows, variables in columns).
#' @param omega  The penalty parameter for ridge penalization (default = 1, meaning no penalization).
#' @param Q      The quantile function to be applied to the copula pseudo-observations (default = qnorm()).
#'
#' @return The (ridge penalized) sample Q-scores rank correlation matrix.
#' @examples
#' # Multivariate normal copula setting

#' q = 10
#' n = 50

#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' sample = mvtnorm::rmvnorm(n,rep(0,q),R,method = "chol") # Sample from multivariate normal distribution
#' omega = cvomega(sample = sample,omegas = seq(0.01,0.999,len = 50),K = 5) # 5-fold cross-validation with Gaussian likelihood as loss for selecting omega
#' R_est = estR(sample,omega = omega)
#'
#' # Multivariate Student-t copula setting

#' q = 10
#' n = 500

#' nu = 7 # Degrees of freedom
#' R2 = function(t,q){(gamma((q+nu)/2)/(((nu-2)^(q/2)) * gamma(nu/2) * gamma(q/2))) *
#'                    (t^((q/2)-1)) * ((1+(t/(nu-2)))^(-(q+nu)/2))} # Density of R^2, with R the radius of the elliptical distribution
#' # Identifiability contraint is that R is the correlation matrix
#' Q = function(t){extraDistr::qlst(t,nu,0,sqrt((nu-2)/nu))} # Univariate quantile function, with unit variance
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' sample = ElliptCopulas::EllDistrSim(n,q,t(chol(R)),density_R2 = function(t){R2(t,q)}) # Sample from multivariate Student-t distribution
#' # with correlation matrix R and nu degrees of freedom
#' R_est = estR(sample,Q = Q)

#' @export


estR = function(sample,omega = 1,Q = function(t){qnorm(t)}){

  n = nrow(sample) # Sample size
  q = ncol(sample) # Total dimension

  scores = matrix(0,n,q) # Matrix for Q-scores

  for(j in 1:q){

    scores[,j] = Q((n/(n+1)) * ecdf(sample[,j])(sample[,j])) # Q-scores

  }

  R_est = cor(scores)

  return(omega * R_est + (1-omega) * diag(q))

}

