#' @title bwd1asR0
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function simulates a sample from the asymptotic distribution of the plug-in estimator for the correlation-based Bures-Wasserstein coefficient \eqn{\mathcal{D}_{1}}
#' between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given that the entire correlation matrix \eqn{\mathbf{R}} is equal to \eqn{\mathbf{R}_{0}} (correlation matrix under independence of \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}).
#' The argument dim should be in ascending order.
#' This function requires importation of the python modules "numpy" and "scipy".
#'
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}, in ascending order.
#' @param M The sample size.
#'
#' @details
#' A sample of size M is drawn from the asymptotic distribution of the plug-in estimator \eqn{\mathcal{D}_{1}(\widehat{\mathbf{R}}_{n})} at \eqn{\mathbf{R}_{0} = \text{diag}(\mathbf{R}_{11}, \dots, \mathbf{R}_{kk})},
#' where \eqn{\widehat{\mathbf{R}}_{n}} is the sample matrix of normal scores rank correlations.
#' The underlying assumption is that the copula of \eqn{\mathbf{X}} is Gaussian.
#'
#' To create a Python virtual environment with "numpy" and "scipy",
#' run:
#'
#' install_tensorflow()
#'
#' reticulate::use_virtualenv("r-tensorflow", required = FALSE)
#'
#' reticulate::py_install("numpy")
#'
#' reticulate::py_install("scipy")
#'
#' @return A sample of size M from the asymptotic distribution of the plug-in estimator for the first Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{1}} under independence of \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' High-dimensional copula-based Wasserstein dependence.
#' doi: https://doi.org/10.48550/arXiv.2404.07141.
#'
#' @seealso \code{\link{bwd1}} for the computation of the first Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{1}},
#'          \code{\link{bwd2}} for the computation of the second Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{2}},
#'          \code{\link{bwd1avar}} for the computation of the asymptotic variance of the plug-in estimator for \eqn{\mathcal{D}_{1}},
#'          \code{\link{bwd2avar}} for the computation of the asymptotic variance of the plug-in estimator for \eqn{\mathcal{D}_{2}},
#'          \code{\link{bwd2asR0}} for sampling from the asymptotic distribution of the plug-in estimator for \eqn{\mathcal{D}_{2}} under the hypothesis of independence between \eqn{\mathbf{X}_{1},\dots,\mathbf{X}_{k}},
#'          \code{\link{estR}} for the computation of the sample matrix of normal scores rank correlations,
#'          \code{\link{otsort}} for rearranging the columns of sample such that dim is in ascending order.
#'
#' @examples
#' \dontrun{
#' q = 5
#' dim = c(2,3)
#'
#' # AR(1) correlation matrix with correlation 0.5
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1)))
#'
#' R0 = createR0(R,dim)
#' sample = bwd1asR0(R0,dim,1000)
#'}
#' @export


bwd1asR0 = function(R, dim, M){

  q = nrow(R) # Total dimension
  k = length(dim) # Amount of random vectors

  eigen_Rii = matrix(0,q,k) # Eigenvales of diagonal blocks Rii
  R_matrices = list() # Rii matrices
  L_matrices = list() # Lambda matrices
  U_matrices = list() # U matrices

  start = 1 # Index corresponding to first position of current random vector

  for(i in 1:k){

    sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector
    R_matrices[[paste("R", i, i, sep = "")]] = R[start:sumdim,start:sumdim]
    eigen = eigen(R[start:sumdim,start:sumdim])
    L_matrices[[paste("L", i, i, sep = "")]] = diag(pmax(eigen$values,0))
    U_matrices[[paste("U", i, i, sep = "")]] = eigen$vectors
    eigen_Rii[1:dim[i],i] = pmax(eigen$values,0)
    start = sumdim + 1 # Update index

  }

  C1 = sum(sqrt(eigen_Rii)) - sum(sqrt(rowSums(eigen_Rii))) # C1
  samples = integer(M) # Sample of size M
  samples_all = array(0, dim = c(k,k,M)) # Samples for each of the blocks

  for(i in 1:(k-1)){

    for(j in (i+1):k){

      Uii = U_matrices[[paste("U", i, i, sep = "")]] ; Lii = L_matrices[[paste("L", i, i, sep = "")]]
      Ujj = U_matrices[[paste("U", j, j, sep = "")]] ; Ljj = L_matrices[[paste("L", j, j, sep = "")]]
      di = nrow(Uii) ; dj = nrow(Ujj)

      if(di == 1){A = matrix(1)} else{A = solve(expm::sqrtm(R_matrices[[paste("R", i, i, sep = "")]]))}
      if(dj == 1){B = matrix(1)} else{B = solve(expm::sqrtm(R_matrices[[paste("R", j, j, sep = "")]]))}
      if(di == 1){Lii = matrix(1)}
      if(dj == 1){Ljj = matrix(1)}

      for(m in 1:M){

        Wij = matrix(stats::rnorm(di * dj),nrow = di, ncol = dj) # Random matrix with normal entries
        Hji = Ujj %*% sqrt(Ljj) %*% t(Wij) %*% sqrt(Lii) %*% t(Uii)
        C = Uii %*% solve(sqrt(Lii)) %*% Wij %*% solve(sqrt(Ljj)) %*% t(Ujj)
        Yij = scipy$linalg$solve_sylvester(A,B,C) # Numerically solve Sylvester equation using Python
        samples_all[i,j,m] = sum(diag(Yij %*% Hji))

      }

      samples = samples + samples_all[i,j,]

    }
  }

  samples = samples/(2*C1)

  return(samples)

}





