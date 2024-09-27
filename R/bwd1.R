#' @title bwd1
#'
#' @description Given a q-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}} dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function computes the correlation-based Bures-Wasserstein coefficient \eqn{\mathcal{D}_{1}} between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}}.
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#'
#' @details
#' Given a correlation matrix \deqn{\mathbf{R} = \begin{pmatrix} \mathbf{R}_{11} & \mathbf{R}_{12} & \cdots & \mathbf{R}_{1k} \\
#'                                                              \mathbf{R}_{12}^{\text{T}} & \mathbf{R}_{22} & \cdots & \mathbf{R}_{2k} \\
#'                                                              \vdots & \vdots & \ddots & \vdots \\
#'                                                              \mathbf{R}_{1k}^{\text{T}} & \mathbf{R}_{2k}^{\text{T}} & \cdots & \mathbf{R}_{kk} \end{pmatrix},}
#' the coefficient \eqn{\mathcal{D}_{1}} equals \deqn{\mathcal{D}_{1}(\mathbf{R}) =
#' \frac{d_{W}^{2}(\mathbf{R},\mathbf{I}_{q}) - \sum_{i=1}^{k}d_{W}^{2}(\mathbf{R}_{ii},\mathbf{I}_{d_{i}})}{\text{sup}_{\mathbf{A} \in \Gamma(\mathbf{R}_{11}, \dots, \mathbf{R}_{kk})}d_{W}^{2}(\mathbf{A},\mathbf{I}_{q}) - \sum_{i=1}^{k}d_{W}^{2}(\mathbf{R}_{ii},\mathbf{I}_{d_{i}})},}
#' where \eqn{d_{W}} stands for the Bures-Wasserstein distance, \eqn{\Gamma(\mathbf{R}_{11}, \dots, \mathbf{R}_{kk})} denotes the set of all correlation matrices
#' with diagonal blocks \eqn{\mathbf{R}_{ii}} for \eqn{i = 1, \dots, k}, and \eqn{\mathbf{I}_{q}} is the identity matrix.
#' The underlying assumption is that the copula of \eqn{\mathbf{X}} is Gaussian.
#'
#' @return The first Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{1}} between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' High-dimensional copula-based Wasserstein dependence.
#' doi: https://doi.org/10.48550/arXiv.2404.07141.
#'
#' @seealso \code{\link{bwd2}} for the computation of the second Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{2}},
#'          \code{\link{bwd1avar}} for the computation of the asymptotic variance of the plug-in estimator for \eqn{\mathcal{D}_{1}}.
#'
#' @examples
#' q = 10
#' dim = c(1,2,3,4)
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' bwd1(R,dim)

#' @export

bwd1 = function(R,dim){

  q = nrow(R) # Total dimension
  k = length(dim) # Amount of random vectors

  eigen_R = pmax(eigen(R)$values,0) # Eigenvalues of R
  eigen_Rii = matrix(0,q,k) # Eigenvalues of diagonal blocks Rii

  start = 1 # Index corresponding to first position of current random vector

  for(i in 1:k){

    sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector
    eigen_Rii[1:dim[i],i] = pmax(eigen(R[start:sumdim,start:sumdim])$values,0)
    start = sumdim + 1 # Update index

  }

  num = sum(sqrt(eigen_Rii)) - sum(sqrt(eigen_R)) # Numerator of D1
  denom = sum(sqrt(eigen_Rii)) - sum(sqrt(rowSums(eigen_Rii))) # Denominator of D1

  return(num/denom)

}
