#' @title bwd1avar
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function computes the asymptotic variance of the plug-in estimator for the correlation-based Bures-Wasserstein coefficient \eqn{\mathcal{D}_{1}}
#' between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}}.
#' The argument dim should be in ascending order.
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}, in ascending order.
#'
#' @details
#' The asymptotic variance of the plug-in estimator \eqn{\mathcal{D}_{1}(\widehat{\mathbf{R}}_{n})} is computed at \eqn{\mathbf{R}},
#' where \eqn{\widehat{\mathbf{R}}_{n}} is the sample matrix of normal scores rank correlations.
#' The underlying assumption is that the copula of \eqn{\mathbf{X}} is Gaussian.
#'
#' @return The asymptotic variance of the plug-in estimator for the first Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{1}} between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' High-dimensional copula-based Wasserstein dependence.
#' doi: https://doi.org/10.48550/arXiv.2404.07141.
#'
#' @seealso \code{\link{bwd1}} for the computation of the first Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{1}},
#'          \code{\link{bwd2}} for the computation of the second Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{2}},
#'          \code{\link{bwd2avar}} for the computation of the asymptotic variance of the plug-in estimator for \eqn{\mathcal{D}_{2}},
#'          \code{\link{bwd1asR0}} for sampling from the asymptotic distribution of the plug-in estimator for \eqn{\mathcal{D}_{1}} under the hypothesis of independence between \eqn{\mathbf{X}_{1},\dots,\mathbf{X}_{k}},
#'          \code{\link{bwd2asR0}} for sampling from the asymptotic distribution of the plug-in estimator for \eqn{\mathcal{D}_{2}} under the hypothesis of independence between \eqn{\mathbf{X}_{1},\dots,\mathbf{X}_{k}},
#'          \code{\link{estR}} for the computation of the sample matrix of normal scores rank correlations,
#'          \code{\link{otsort}} for rearranging the columns of sample such that dim is in ascending order.
#'
#' @examples
#' q = 10
#' dim = c(1,2,3,4)
#'
#' # AR(1) correlation matrix with correlation 0.5
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1)))
#'
#' bwd1avar(R,dim)

#' @export

bwd1avar = function(R, dim){

  q = nrow(R) # Total dimension
  k = length(dim) # Amount of random vectors

  eigen_Rii = matrix(0,q,k) # Eigenvalues of diagonal blocks Rii
  L_matrices = list() # Lambda matrices
  U_matrices = list() # U matrices
  D1_matrices = list() # Delta matrices

  start = 1 # Index corresponding to first position of current random vector

  # Compute eigenvalues (Lambda matrices) and eigenvectors (U matrices)

  for(i in 1:k){

    sumdim = sum(dim[1:i]) # Index of last position of current random vector
    eigen = eigen(R[start:sumdim,start:sumdim])
    L_matrices[[paste("L", i, i, sep = "")]] = diag(pmax(eigen$values,0))
    U_matrices[[paste("U", i, i, sep = "")]] = eigen$vectors
    eigen_Rii[1:dim[i],i] = pmax(eigen$values,0)
    start = sumdim + 1 # Update index

  }

  # Compute Delta matrices

  sum = 0

  for(j in 1:k){sum = sum + L_matrices[[paste("L", j, j, sep = "")]][1:dim[1],1:dim[1]]}

  D1_matrices[["D1_1"]] = Re(expm::sqrtm(solve(sum))) # Delta1

  for(i in 2:k){ # Deltai for i = 2,...,k

    if(dim[i] == dim[i-1]){

      D1_matrices[[paste("D1_", i, sep = "")]] = D1_matrices[[paste("D1_", i-1, sep = "")]]

    } else{

      sum = 0

      for(j in i:k){sum = sum + L_matrices[[paste("L", j, j, sep = "")]][(dim[i-1]+1):dim[i],(dim[i-1]+1):dim[i]]}
      D1_matrices[[paste("D1_", i, sep = "")]] = magic::adiag(D1_matrices[[paste("D1_", i-1, sep = "")]],
                                                              Re(expm::sqrtm(solve(sum))))

    }
  }

  R0 = createR0(R,dim) # R0 matrix
  sqrtR0 = Re(expm::sqrtm(R0)) # Square root of R0
  invsqrtR0 = solve(sqrtR0) # Inverse square root of R0

  C1 = sum(sqrt(eigen_Rii)) - sum(sqrt(rowSums(eigen_Rii))) # C1
  Ups1 = U_matrices[["U11"]] %*% D1_matrices[["D1_1"]] %*% t(U_matrices[["U11"]]) # First block of Upsilon1

  for(i in 2:k){ # Construct other blocks of Upsilon1
    Ups1 = magic::adiag(Ups1,U_matrices[[paste("U", i, i, sep = "")]] %*% D1_matrices[[paste("D1_", i, sep = "")]] %*% t(U_matrices[[paste("U", i, i, sep = "")]]))
  }

  D1 = bwd1(R,dim)
  M1 = (1/(2*C1)) * (-Re(expm::sqrtm(solve(R))) + (1-D1) * invsqrtR0 + D1 * Ups1) # Matrix for Fréchet derivative
  zeta1H = R %*% (M1 - diag(diag(M1 %*% R)))
  zeta1Squared = 2 * sum(diag(zeta1H %*% zeta1H))

  return(zeta1Squared)

}
