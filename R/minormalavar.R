#' @title minormalavar
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function computes the asymptotic variance of the plug-in estimator for the correlation-based mutual information
#' between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}}.
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#'
#' @details
#' The asymptotic variance of the plug-in estimator \eqn{\mathcal{D}_{t \ln(t)}(\widehat{\mathbf{R}}_{n})} is computed at \eqn{\mathbf{R}},
#' where \eqn{\widehat{\mathbf{R}}_{n}} is the sample matrix of normal scores rank correlations.
#' The underlying assumption is that the copula of \eqn{\mathbf{X}} is Gaussian.
#'
#' @return The asymptotic variance of the correlation-based mutual information between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' Parametric dependence between random vectors via copula-based divergence measures.
#' Journal of Multivariate Analysis 203:105336.
#' doi: https://doi.org/10.1016/j.jmva.2024.105336.
#'
#' @seealso \code{\link{minormal}} for the computation of the mutual information,
#'          \code{\link{Helnormal}} for the computation of the Hellinger distance,
#'          \code{\link{Helnormalavar}} for the computation of the asymptotic variance of the plug-in estimator for the Hellinger distance,
#'          \code{\link{estR}} for the computation of the sample matrix of normal scores rank correlations.
#'
#' @examples
#' q = 10
#' dim = c(1,2,3,4)
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' minormalavar(R,dim)

#' @export


minormalavar = function(R, dim){

  R0 = createR0(R,dim) # R0 matrix
  M = (1/2) * (solve(R0) - solve(R)) # Matrix for Fréchet derivative
  zetaH = R %*% (M - diag(diag(M %*% R)))
  zetaSquared = 2 * sum(diag(zetaH %*% zetaH))

  return(zetaSquared)

}


