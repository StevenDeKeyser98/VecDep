#' @title minormal
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function computes the correlation-based mutual information between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}}.
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#'
#' @details
#' Given a correlation matrix \deqn{\mathbf{R} = \begin{pmatrix} \mathbf{R}_{11} & \mathbf{R}_{12} & \cdots & \mathbf{R}_{1k} \\
#'                                                              \mathbf{R}_{12}^{\text{T}} & \mathbf{R}_{22} & \cdots & \mathbf{R}_{2k} \\
#'                                                              \vdots & \vdots & \ddots & \vdots \\
#'                                                              \mathbf{R}_{1k}^{\text{T}} & \mathbf{R}_{2k}^{\text{T}} & \cdots & \mathbf{R}_{kk} \end{pmatrix},}
#' the mutual information equals \deqn{\mathcal{D}_{t \ln(t)}^{\mathcal{N}}(\mathbf{R}) = - \frac{1}{2} \ln \left (\frac{|\mathbf{R}|}{\prod_{i = 1}^{k} \left |\mathbf{R}_{ii} \right |} \right ).}
#' The underlying assumption is that the copula of \eqn{\mathbf{X}} is Gaussian.
#'
#' @return The correlation-based mutual information between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' Parametric dependence between random vectors via copula-based divergence measures.
#' Journal of Multivariate Analysis 203:105336.
#' doi: https://doi.org/10.1016/j.jmva.2024.105336.
#'
#' @seealso \code{\link{Helnormal}} for the computation of the Gaussian copula Hellinger distance,
#'          \code{\link{minormalavar}} for the computation of the asymptotic variance of the plug-in estimator for the Gaussian copula mutual information,
#'          \code{\link{miStudent}} for the computation of the Student-t mutual information.
#'
#' @examples
#' q = 10
#' dim = c(1,2,3,4)
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' minormal(R,dim)

#' @export

minormal = function(R, dim){

  R0 = createR0(R,dim) # Correlation matrix under independence of X_1,...,X_k

  return((-1/2) * log(det(R)/det(R0)))

}
