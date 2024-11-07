#' @title phinp
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function estimates the \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} by estimating the joint and marginal
#' copula densities via fully non-parametric copula kernel density estimation.
#'
#' @param sample A sample from a \eqn{q}-dimensional random vector \eqn{\mathbf{X}} (\eqn{n \times q} matrix with observations in rows, variables in columns).
#' @param cop A fitted reference hac object, in case bw_method = 0 (default = NULL).
#' @param dim The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#' @param phi The function \eqn{\Phi}.
#' @param estimator Either ``beta'' or ``trans'' for the beta kernel or the Gaussian transformation kernel copula density estimator.
#' @param bw_method A number in \eqn{\{0,1,2\}} specifying the method used for computing optimal local bandwidths.
#'
#' @details
#' When \eqn{\mathbf{X}} has copula density \eqn{c} with marginal copula densities \eqn{c_{i}} of \eqn{\mathbf{X}_{i}} for \eqn{i = 1, \dots, k},
#' the \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}} equals
#' \deqn{\mathcal{D}_{\Phi} \left (\mathbf{X}_{1}, \dots, \mathbf{X}_{k} \right ) = \mathbb{E} \left \{ \frac{\prod_{i = 1}^{k} c_{i}(\mathbf{U}_{i})}{c \left ( \mathbf{U} \right )} \Phi \left (\frac{c(\mathbf{U})}{\prod_{i = 1}^{k}c_{i}(\mathbf{U}_{i})} \right ) \right \},}
#' for a certain continuous, convex function \eqn{\Phi : (0,\infty) \rightarrow \mathbb{R}}, and with \eqn{\mathbf{U} = (\mathbf{U}_{1}, \dots, \mathbf{U}_{k}) \sim c}.
#'
#' The expectation \eqn{\mathbb{E}} is replaced by the empirical mean using the estimated copula sample \eqn{\widehat{\mathbf{U}}^{(1)}, \dots, \widehat{\mathbf{U}}^{(n)}} with \eqn{\widehat{\mathbf{U}}^{(\ell)} = (\widehat{\mathbf{U}}_{1}^{(\ell)}, \dots, \widehat{\mathbf{U}}_{k}^{(\ell)})} for \eqn{\ell = 1, \dots, n}, where (recall that \eqn{\mathbf{X}_{i} = (X_{i1}, \dots, X_{id_{i}})} for \eqn{i = 1, \dots, k})
#' \deqn{\widehat{\mathbf{U}}_{i}^{(\ell)} = \left (\widehat{U}_{i1}^{(\ell)}, \dots, \widehat{U}_{id_{i}}^{(\ell)} \right ) = \left (\widehat{F}_{i1} \left (X_{i1}^{(\ell)} \right ), \dots, \widehat{F}_{id_{i}} \left (X_{id_{i}}^{(\ell)} \right )  \right ).}
#' Hereby, \eqn{\widehat{F}_{ij}(x_{ij}) = \frac{1}{n+1} \sum_{\ell = 1}^{n} 1 \left (X_{ij}^{(\ell)} \leq x_{ij} \right )} is the (rescaled) empirical cdf of \eqn{X_{ij}} based on a sample \eqn{X_{ij}^{(1)}, \dots, X_{ij}^{(n)}} for \eqn{i = 1, \dots, k} and \eqn{j = 1, \dots, d_{i}}.
#'
#' The joint copula density \eqn{c} and marginal copula densities \eqn{c_{i}} for \eqn{i = 1, \dots, k} are estimated via fully non-parametric copula kernel density estimation.
#' When estimator = "beta", the beta kernel copula density estimator is used.
#' When estimator = "trans", the Gaussian transformation kernel copula density estimator is used.
#'
#' Bandwidth selection is done locally by using the function \code{\link{hamse}}.
#' When bw_method = 0, then the given fitted (e.g., via MLE using \code{\link{mlehac}}) hac object (hierarchical Archimedean copula) cop is used as reference copula.
#' When bw_method = 1, then a non-parametric (beta or Gaussian transformation) kernel copula density estimator based on the pseudos as pivot is used. This pivot is computed
#' using the big O bandwidth (i.e., \eqn{n^{-2/(q+4)}} in case of the beta estimator, and \eqn{n^{-1/(q+4)}} for the transformation estimator, with \eqn{q} the total dimension).
#' When bw_method = 2, the big O bandwidths are taken.
#'
#' @return The estimated \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}}.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' Hierarchical variable clustering via copula-based divergence measures between random vectors.
#' International Journal of Approximate Reasoning 165:109090.
#' doi: https://doi.org/10.1016/j.ijar.2023.109090.
#'
#' @seealso \code{\link{betakernelestimator}} for the computation of the beta kernel copula density estimator, \cr
#'          \code{\link{transformationestimator}} for the computation of the Gaussian transformation kernel copula density estimator,
#'          \code{\link{hamse}} for local bandwidth selection for the beta kernel or Gaussian transformation kernel copula density estimator.
#'
#'
#' @examples
#' \dontrun{
#' q = 4
#' dim = c(2,2)
#'
#' # Sample size
#' n = 500
#'
#' # Four dimensional hierarchical Gumbel copula
#' # with parameters (theta_0,theta_1,theta_2) = (2,3,4)
#' HAC = gethac(dim,c(2,3,4),type = 1)
#'
#' # Sample
#' sample = HAC::rHAC(n,HAC)
#'
#' # Maximum pseudo-likelihood estimator to be used as reference copula for bw_method = 0
#' est_cop = mlehac(sample,dim,1,c(2,3,4))
#'
#' # Estimate mutual information between two random vectors of size 2 in different ways
#'
#' est_phi_1 = phinp(sample,cop = est_cop,dim = dim,phi = function(t){t * log(t)},
#'                   estimator = "beta",bw_method = 0)
#' est_phi_2 = phinp(sample,cop = est_cop,dim = dim,phi = function(t){t * log(t)},
#'                   estimator = "trans",bw_method = 0)
#' est_phi_3 = phinp(sample,dim = dim,phi = function(t){t * log(t)},
#'                   estimator = "beta",bw_method = 1)
#' est_phi_4 = phinp(sample,dim = dim,phi = function(t){t * log(t)},
#'                   estimator = "trans",bw_method = 1)
#' est_phi_5 = phinp(sample,dim = dim,phi = function(t){t * log(t)},
#'                   estimator = "beta",bw_method = 2)
#' est_phi_6 = phinp(sample,dim = dim,phi = function(t){t * log(t)},
#'                   estimator = "trans",bw_method = 2)
#'}
#'
#' @export


phinp = function(sample, cop = NULL, dim, phi, estimator, bw_method){

  n = nrow(sample)
  q = ncol(sample)
  k = length(dim)

  pseudos = matrix(0,n,q) # Pseudo copula observations

  for(j in 1:q){pseudos[,j] = (n/(n+1)) * stats::ecdf(sample[,j])(sample[,j])}

  h_joint = integer(n) # Bandwidths for q-dimensional observations
  h_child = matrix(0,n,k) # Bandwidths for d_i-dimensional observations for i = 1,...,k

  if(!is.null(cop)){

    childs = childhac(cop) # Child NAC copulas for marginal bandwidths
    pseudosfp = NULL # No pseudo observations needed for bandwidths in this case

  } else{

    childs = NULL # No child NAC copulas in this case
    pseudosfp = pseudos # Pseudo observations needed for bandwidths in this case

  }

  for(l in 1:n){

    if(bw_method == 2){

      if(estimator == "beta"){

        h_joint[l] = n^(-2/(q+4))

      }

      if(estimator == "trans"){

        h_joint[l] = n^(-1/(q+4))

      }

    } else{

      h_joint[l] = hamse(pseudos[l,],cop,pseudosfp,n,estimator,bw_method)

    }

    start = 1 # Index corresponding to first position of current random vector

    for(i in 1:k){

      sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector

      if(length(start:sumdim) == 1){

        h_child[l,i] = 1 # Bandwidth does not matter when child is one dimensional

      } else{

        if(bw_method == 2){

          if(estimator == "beta"){

            h_child[l,i] = n^(-2/(length(start:sumdim) + 4))

          }

          if(estimator == "trans"){

            h_child[l,i] = n^(-1/(length(start:sumdim) + 4))

          }

        } else{

          h_child[l,i] = hamse(pseudos[l,start:sumdim],childs[[i]],pseudosfp[,start:sumdim],n,estimator,bw_method)

        }
      }

      start = sumdim + 1 # Update index

    }
  }

  dep = integer(n) # integrands of phi-dependence

  for(l in 1:n){

    if(estimator == "beta"){

      joint_dens = betakernelestimator(pseudos[l,],h_joint[l],pseudos)

    }

    if(estimator == "trans"){

      joint_dens = transformationestimator(pseudos[l,],h_joint[l],pseudos)

    }

    child_dens = 1 # Product of marginal estimated copula densities

    start = 1 # Index corresponding to first position of current random vector

    for(i in 1:k){

      sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector

      if(length(start:sumdim) == 1){

        child_dens = child_dens

      } else{

        if(estimator == "beta"){

          child_dens = child_dens * betakernelestimator(pseudos[l,start:sumdim],h_child[l,i],pseudos[,start:sumdim])

        }

        if(estimator == "trans"){

          child_dens = child_dens * transformationestimator(pseudos[l,start:sumdim],h_child[l,i],pseudos[,start:sumdim])

        }
      }

      start = sumdim + 1 # Update index

    }

    dep[l] = (child_dens/joint_dens) * phi(joint_dens/child_dens) # phi dependence integrand
  }

  dep = dep[!is.na(dep) & !is.infinite(dep)]

  return(mean(dep))

}

