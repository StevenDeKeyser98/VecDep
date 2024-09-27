#' @title createR0
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function constructs the correlation matrix under independence of \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}, given the entire correlation matrix \eqn{\mathbf{R}}.
#'
#' @param R  The correlation matrix of \eqn{\mathbf{X}}.
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#'
#' @details Given a correlation matrix \deqn{\mathbf{R} = \begin{pmatrix} \mathbf{R}_{11} & \mathbf{R}_{12} & \cdots & \mathbf{R}_{1k} \\
#'                                                              \mathbf{R}_{12}^{\text{T}} & \mathbf{R}_{22} & \cdots & \mathbf{R}_{2k} \\
#'                                                              \vdots & \vdots & \ddots & \vdots \\
#'                                                              \mathbf{R}_{1k}^{\text{T}} & \mathbf{R}_{2k}^{\text{T}} & \cdots & \mathbf{R}_{kk} \end{pmatrix},}
#'         the matrix \eqn{\mathbf{R}_{0} = \text{diag}(\mathbf{R}_{11}, \dots, \mathbf{R}_{kk})}, being the correlation matrix
#'         under independence of \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}}, is returned.
#'
#' @return The correlation matrix under independence of \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{n}}.
#' @examples
#' q = 10
#' dim = c(1,2,3,4)
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
#' createR0(R,dim)

#' @export

createR0 = function(R, dim){

  R0 = matrix(0,nrow(R),ncol(R)) # Initialize matrix of zeroes

  start = 1 # Index corresponding to first position of current random vector

  for(i in 1:length(dim)){

    sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector
    R0[start:sumdim,start:sumdim] = R[start:sumdim,start:sumdim] # Diagonal block
    start = sumdim + 1 # Update index

  }

  return(R0)

}
