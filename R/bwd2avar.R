#' @title bwd2avar
#'
#' @description Given a q-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}} dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function computes the asymptotic variance of the correlation-based Bures-Wasserstein coefficient \eqn{\mathcal{D}_{2}}
#' between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}}.
#' The argument dim should be in ascending order.
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}, in ascending order.
#'
#' @return The asymptotic variance of the second Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{2}} between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#' @examples
#' q = 10
#' dim = c(1,2,3,4)
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' bwd2avar(R,dim)

#' @export

bwd2avar = function(R,dim){

  q = nrow(R) # Total dimension
  k = length(dim) # Amount of random vectors

  eigen_Rii = matrix(0,q,k) # Eigenvalues of diagonal blocks Rii
  L_matrices = list() # Lambda matrices
  U_matrices = list() # U matrices
  D2_matrices = list() # Delta tilde matrices

  start = 1 # Index corresponding to first position of current random vector

  # Compute eigenvalues (Lambda matrices) and eigenvectors (U matrices)

  for(i in 1:k){

    sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector
    eigen = eigen(R[start:sumdim,start:sumdim])
    L_matrices[[paste("L", i, i, sep = "")]] = diag(pmax(eigen$values,0))
    U_matrices[[paste("U", i, i, sep = "")]] = eigen$vectors
    eigen_Rii[1:dim[i],i] = pmax(eigen$values,0)
    start = sumdim + 1 # Update index

  }

  # Compute Delta tilde matrices

  sum = 0

  for(j in 1:k){sum = sum + L_matrices[[paste("L", j, j, sep = "")]][1:dim[1],1:dim[1]]^2}

  D2_matrices[["D2_1"]] = Re(expm::sqrtm(solve(sum))) %*% L_matrices[["L11"]][1:dim[1],1:dim[1]] # Delta tilde1

  for(i in 2:k){ # Delta tildei for i = 2,...,k

    D = solve(L_matrices[[paste("L", i-1, i-1, sep = "")]][1:dim[1],1:dim[1]]) %*% L_matrices[[paste("L", i, i, sep = "")]][1:dim[1],1:dim[1]]

    if(i > 2){

      for(j in 2:(i-1)){

        if(dim[j] != dim[j-1]){
          D = magic::adiag(D,solve(L_matrices[[paste("L", i-1, i-1, sep = "")]][(dim[j-1]+1):dim[j],(dim[j-1]+1):dim[j]]) %*%
                             L_matrices[[paste("L", i, i, sep = "")]][(dim[j-1]+1):dim[j],(dim[j-1]+1):dim[j]])
        }
      }
    }

    if(dim[i] == dim[i-1]){
      D2_matrices[[paste("D2_", i, sep = "")]] = D2_matrices[[paste("D2_", i-1, sep = "")]] %*% D

    } else{
      sum = 0

      for(j in i:k){sum = sum + L_matrices[[paste("L", j, j, sep = "")]][(dim[i-1]+1):dim[i],(dim[i-1]+1):dim[i]]^2}

      D2_matrices[[paste("D2_", i, sep = "")]] = magic::adiag(D2_matrices[[paste("D2_", i-1, sep = "")]] %*% D,
                                                              Re(expm::sqrtm(solve(sum))) %*%
                                                                L_matrices[[paste("L", i, i, sep = "")]][(dim[i-1]+1):dim[i],(dim[i-1]+1):dim[i]])
    }
  }

  R0 = createR0(R,dim) # R0 matrix
  sqrtR0 = Re(expm::sqrtm(R0)) # Square root of R0
  invsqrtR0 = solve(sqrtR0) # Inverse square root of R0
  J = invsqrtR0 %*% Re(expm::sqrtm(sqrtR0 %*% R %*% sqrtR0)) %*% invsqrtR0 # J
  J0 = createR0(J,dim) # J0

  C2 = q - sum(sqrt(rowSums(eigen_Rii^2))) # C2
  Ups2 = U_matrices[["U11"]] %*% D2_matrices[["D2_1"]] %*% t(U_matrices[["U11"]]) # First block of Upsilon2

  for(i in 2:k){ # Construct other blocks of Upsilon2

    Ups2 = magic::adiag(Ups2,U_matrices[[paste("U", i, i, sep = "")]] %*% D2_matrices[[paste("D2_", i, sep = "")]] %*% t(U_matrices[[paste("U", i, i, sep = "")]]))

  }

  D2 = bwd2(R,dim)
  M2 = (1/C2) * ((-1/2) * (J0 + solve(J)) + (1-D2) * diag(rep(1,q)) + D2 * Ups2)
  zeta2H = R %*% (M2 - diag(diag(M2 %*% R)))
  zeta2Squared = 2 * sum(diag(zeta2H %*% zeta2H))

  return(zeta2Squared)

}
