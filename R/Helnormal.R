#' @title Helnormal
#'
#' @description Given a q-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_i} a \eqn{d_{i}} dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function computes the correlation-based Hellinger distance between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}}.
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#'
#' @return The correlation-based Hellinger distance between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#' @examples
#' q = 10
#' dim = c(1,2,3,4)
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' Helnormal(R,dim)

#' @export


Helnormal = function(R,dim){

  q = nrow(R) # Total dimension

  B = solve(createR0(R,dim)) # Inverse of R0
  det = det(diag(q) + B %*% R)

  return(2 - 2 * (sqrt((2^q)/det) * exp((-1/2) * minormal(R,dim))))

}


