#' @title miStudent
#'
#' @description Given a q-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}} dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function computes the Student-t mutual information between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}} and the degrees of freedom nu.
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#' @param nu The degrees of freedom.
#'
#' @return The Student-t mutual information between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#' @examples
#' q = 10
#' dim = c(1,2,3,4)
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' nu = 7
#' miStudent(R,dim,nu)

#' @export


miStudent = function(R,dim,nu){

  q = sum(dim) # Total dimension
  k = length(dim) # Amount of random vectors

  # Terms T1,T2,T3 depending on the degrees of freedom

  T1 = log((gamma((q+nu)/2) * (gamma(nu/2)^(k-1)))/prod(gamma((dim+nu)/2)))
  T2 = sum(((dim+nu)/2) * digamma((dim+nu)/2))
  T3 = -((q+nu)/2) * digamma((q+nu)/2) - (nu/2) * (k-1) * digamma(nu/2)

  return(minormal(R,dim) + T1 + T2 + T3)

}


