#' @title bwd2asR0
#'
#' @description Given a q-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}} dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function simulates a sample from the asymptotic distribution of the correlation-based Bures-Wasserstein coefficient \eqn{\mathcal{D}_{2}}
#' between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}} is equal to \eqn{\mathbf{R}_{0}} (correlation matrix under independence of \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}).
#' The argument dim should be in ascending order.
#' This function requires importation of the python modules "numpy" and "scipy".

#'
#' @param R  TThe correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}, in ascending order.
#' @param M The sample size.
#'
#' @return A sample of size M from the asymptotic distribution of the second Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{2}} under independence of \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#' @examples
#' q = 5
#' dim = c(2,3)
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' R0 = createR0(R,dim)
#' sample = bwd2asR0(R0,dim,1000)

#' @export


bwd2asR0 = function(R,dim,M){

  q = nrow(R) # Total dimension
  k = length(dim) # Amount of random vectors

  eigen_Rii = matrix(0,q,k) # Eigenvalues of diagonal blocks Rii
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

  C2 = q - sum(sqrt(rowSums(eigen_Rii^2))) # C2
  samples = integer(M) # Sample of size M
  samples_all = array(0, dim = c(k,k,M)) # Samples for each of the blocks

  for(i in 1:(k-1)){

    for(j in (i+1):k){

      Uii = U_matrices[[paste("U", i, i, sep = "")]] ; Lii = L_matrices[[paste("L", i, i, sep = "")]]
      Ujj = U_matrices[[paste("U", j, j, sep = "")]] ; Ljj = L_matrices[[paste("L", j, j, sep = "")]]
      di = nrow(Uii) ; dj = nrow(Ujj)

      if(di == 1){A = matrix(1)} else{A = R_matrices[[paste("R", i, i, sep = "")]]}
      if(dj == 1){B = matrix(1)} else{B = R_matrices[[paste("R", j, j, sep = "")]]}
      if(di == 1){Lii = matrix(1)}
      if(dj == 1){Ljj = matrix(1)}

      for(m in 1:M){

        print(m)
        Wij = matrix(rnorm(di * dj),nrow = di, ncol = dj) # Random matrix with normal entries
        C = Uii %*% Lii %*% Wij %*% Ljj %*% t(Ujj)
        Kij = scipy$linalg$solve_sylvester(A,B,C)
        samples_all[i,j,m] = sum(diag(t(Uii) %*% Kij %*% Ujj %*% t(Wij))) # Numerically solve Sylvester equation using Python

      }

      samples = samples + samples_all[i,j,]

    }
  }

  samples = samples/(2*C2)

  return(samples)

}
