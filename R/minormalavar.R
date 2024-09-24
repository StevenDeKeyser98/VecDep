#' @title minormalavar
#'
#' @description Given a q-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_i} a \eqn{d_{i}} dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function computes the asymptotic variance of the correlation-based mutual information
#' between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}}.
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}, in ascending order.
#'
#' @return The asymptotic variance of the correlation-based mutual information between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#' @examples
#' q = 10
#' dim = c(1,2,3,4)
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' minormalavar(R,dim)

#' @export


minormalavar = function(R,dim){

  R0 = createR0(R,dim) # R0 matrix
  M = (1/2) * (solve(R0) - solve(R)) # Matrix for Fréchet derivative
  zetaH = R %*% (M - diag(diag(M %*% R)))
  zetaSquared = 2 * sum(diag(zetaH %*% zetaH))

  return(zetaSquared)

}


