#' @title miStudent
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function computes the Student-t mutual information between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}} and the degrees of freedom nu.
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#' @param nu The degrees of freedom.
#'
#' @details
#' Given a correlation matrix \deqn{\mathbf{R} = \begin{pmatrix} \mathbf{R}_{11} & \mathbf{R}_{12} & \cdots & \mathbf{R}_{1k} \\
#'                                                              \mathbf{R}_{12}^{\text{T}} & \mathbf{R}_{22} & \cdots & \mathbf{R}_{2k} \\
#'                                                              \vdots & \vdots & \ddots & \vdots \\
#'                                                              \mathbf{R}_{1k}^{\text{T}} & \mathbf{R}_{2k}^{\text{T}} & \cdots & \mathbf{R}_{kk} \end{pmatrix},}
#' and a certain amount of degrees of freedom \eqn{\nu > 0},
#' the Student-t mutual information equals \deqn{\mathcal{D}_{t \ln(t)}^{\text{S}}(\mathbf{R},\nu) = - \frac{1}{2} \ln \left (\frac{|\mathbf{R}|}{\prod_{i = 1}^{k} \left |\mathbf{R}_{ii} \right |} \right ) + K(\nu),}
#' where
#' \deqn{K(\nu) = \ln \left (\frac{\Gamma((q+\nu)/2) \Gamma(\nu/2)^{k-1}}{\prod_{i = 1}^{k} \Gamma((d_{i} + \nu)/2)} \right ) + \sum_{i = 1}^{k} \left [\frac{d_{i} + \nu}{2} \psi((d_{i} + \nu)/2) \right ] - \frac{q + \nu}{2} \psi((q + \nu)/2) - \frac{\nu}{2}(k-1)\psi(\nu/2),}
#' with \eqn{\Gamma} the gamma function and \eqn{\psi} the digamma function.
#' The underlying assumption is that the copula of \eqn{\mathbf{X}} is Student-t.
#'
#' @return The Student-t mutual information between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' Hierarchical variable clustering via copula-based divergence measures between random vectors.
#' International Journal of Approximate Reasoning 165:109090.
#' doi: https://doi.org/10.1016/j.ijar.2023.109090.
#'
#' @seealso \code{\link{minormal}} for the computation of the Gaussian copula mutual information.
#'
#' @examples
#' q = 10
#' dim = c(1,2,3,4)
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' nu = 7
#' miStudent(R,dim,nu)

#' @export


miStudent = function(R, dim, nu){

  q = sum(dim) # Total dimension
  k = length(dim) # Amount of random vectors

  # Terms T1,T2,T3 depending on the degrees of freedom

  T1 = log((gamma((q+nu)/2) * (gamma(nu/2)^(k-1)))/prod(gamma((dim+nu)/2)))
  T2 = sum(((dim+nu)/2) * digamma((dim+nu)/2))
  T3 = -((q+nu)/2) * digamma((q+nu)/2) - (nu/2) * (k-1) * digamma(nu/2)

  return(minormal(R,dim) + T1 + T2 + T3)

}


