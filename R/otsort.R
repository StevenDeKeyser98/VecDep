#' @title otsort
#'
#' @description Given a q-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_i} a \eqn{d_{i}} dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function sorts the columns (variables) of a sample of \eqn{\mathbf{X}} such that the dimensions are in ascending order.
#'
#' @param sample A sample from a q-dimensional random vector \eqn{\mathbf{X}} (n x q matrix with observations in rows, variables in columns).
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}, in order as given in sample.
#'
#' @return A list with elements "sample" containing the ordered sample, and "dim" containing the ordered dimensions.
#' @examples
#' q = 10
#' n = 50
#' dim = c(2,3,1,4)
#' sample = mvtnorm::rmvnorm(n,rep(0,q),diag(q),method = "chol") # Sample from multivariate normal distribution
#' ordered = otsort(sample,dim)

#' @export


otsort = function(sample,dim){

  k = length(dim) # Amount of random vectors

  dim_new = sort(dim) # Sorted dimensions
  sample_new = sample # Copy of sample
  order = order(dim) # Order of dimensions (first element = position of smallest dimension, last element = position of largest dimension)

  start = 1 # Index corresponding to first position of the current random vector

  for(i in 1:k){

    sumdim = sum(dim_new[1:i]) # Index corresponding to last position of the current random vector
    pick = sum(dim[1:order[i]]) - dim[order[i]] + 1 # First column index of current random vector
    sample_new[,start:sumdim] = sample[,pick:(pick + dim[order[i]] - 1)] # Create ordered sample
    start = sumdim + 1 # Update index

  }

  return(list("sample" = sample_new, "dim" = dim_new))

}
