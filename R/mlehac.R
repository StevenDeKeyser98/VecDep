#' @title mlehac
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function performs maximum pseudo-likelihood estimation for the parameters of
#' a hierarchical Archimedean copula with two nesting levels of a specific type, used for modelling the dependence between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}}.
#'
#' @param sample A sample from a \eqn{q}-dimensional random vector \eqn{\mathbf{X}} (\eqn{n \times q} matrix with observations in rows, variables in columns).
#' @param dim The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#' @param type The type of Archimedean copula.
#' @param start_val The starting values for the parameters \eqn{(\theta_{0},\theta_{1},...,\theta_{k})} of the \cr
#'                  hierarchical Archimedean copula.
#'
#' @details
#' Under the assumption that \eqn{\mathbf{X} = (\mathbf{X}_{1}, \dots, \mathbf{X}_{k})} has a hierarchical Archimedean copula with two nesting levels, i.e.,
#' \deqn{C(\mathbf{u}) = C_{0} \left (C_{1}(\mathbf{u}_{1}), \dots, C_{k}(\mathbf{u}_{k}) \right ),}
#' where \eqn{\mathbf{u} = (\mathbf{u}_{1}, \dots, \mathbf{u}_{k}) \in \mathbb{R}^{q}} with \eqn{\mathbf{u}_{i} \in \mathbb{R}^{d_{i}}} for \eqn{i = 1, \dots, k},
#' and with \eqn{\theta_{i}} the parameter of \eqn{C_{i}} for \eqn{i = 0,1, \dots, k} (see the function \code{\link{gethac}}), this functions performs maximum pseudo-likelihood estimation for
#' \eqn{\boldsymbol{\theta}_{C} = (\theta_{0}, \theta_{1}, \dots, \theta_{k})}. This means that for \eqn{\widehat{F}_{ij}(x_{ij}) = \frac{1}{n+1} \sum_{\ell = 1}^{n} 1 \left (X_{ij}^{(\ell)} \leq x_{ij} \right )} the (rescaled) empirical cdf of \eqn{X_{ij}} based on a sample \eqn{X_{ij}^{(1)}, \dots, X_{ij}^{(n)}}
#' for \eqn{i = 1, \dots, k} and \eqn{j = 1, \dots, d_{i}} (recall that \eqn{\mathbf{X}_{i} = (X_{i1}, \dots, X_{id_{i}})}),
#' we look for \deqn{\widehat{\boldsymbol{\theta}}_{C,n}^{\text{NP}} = \text{arg max}_{\boldsymbol{\theta}_{C}} \sum_{\ell = 1}^{n} \ln \left \{c \left ( \widehat{F}_{11} \left (X_{11}^{(\ell)} \right ), \dots, \widehat{F}_{kd_{k}} \left (X_{kd_{k}}^{(\ell)} \right ) ; \boldsymbol{\theta}_{C} \right ) \right \},}
#' where \eqn{c( \cdot ; \boldsymbol{\theta}_{C})} is the copula density of the hierarchical Archimedean copula.
#'
#' We assume that \eqn{C_{i}} belongs to the same family of Archimedean copulas (e.g., Clayton) for \eqn{i = 0, \dots, k},
#' and make use of the R package \sQuote{HAC}.
#'
#' In case the starting values (start_val) are not specified, the starting value for \eqn{\theta_{0}} is put equal to 1.9
#' and the starting values for \eqn{\theta_{i}} with \eqn{i \in \{1, \dots, k \}} are determined by performing
#' maximum pseudo-likelihood estimation to the \eqn{d_{i}}-dimensional marginals with starting value \eqn{2}.
#'
#' @return The maximum pseudo-likelihood estimates for \eqn{(\theta_{0},\theta_{1}, \dots, \theta_{k})}.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' Parametric dependence between random vectors via copula-based divergence measures.
#' Journal of Multivariate Analysis 203:105336. \cr
#' doi: https://doi.org/10.1016/j.jmva.2024.105336.
#'
#' Okhrin, O., Ristig, A. & Chen, G. (2024).
#' HAC: estimation, simulation and visualization of hierarchical Archimedean copulae (HAC), R package version 1.1-1. \cr
#' url:  https://CRAN.R-project.org/package=HAC.
#'
#' @seealso \code{\link{gethac}} for creating a hac object with two nesting levels,
#'          \code{\link{phihac}} for computing the \eqn{\Phi}-dependence between all the child copulas of a hac object with two nesting levels,
#'          \code{\link{Helhac}} for computing the Hellinger distance between all the child copulas of a hac object with two nesting levels.
#'
#' @examples
#' dim = c(2,2)
#' thetas = c(2,3,4)
#'
#' # Sample size
#' n = 1000
#'
#' # 4 dimensional nested Gumbel copula with (theta_0,theta_1,theta_2) = (2,3,4)
#' HAC = gethac(dim,thetas,type = 1)
#'
#' # Sample
#' sample = suppressWarnings(HAC::rHAC(n,HAC))
#'
#' # Maximum pseudo-likelihood estimator with starting values equal to thetas
#' HAC_est_1 = mlehac(sample,dim,1,thetas)
#'
#' # Maximum pseudo-likelihood estimator with starting values
#' # theta_0 = 1.9, and theta_1, theta_2 determined by maximum
#' # pseudo-likelihood estimation for marginal child copulas
#'
#' HAC_est_2 = mlehac(sample,dim,1)
#'
#'
#' @export


mlehac = function(sample, dim, type, start_val = NULL){

  if(!is.null(start_val)){ # In case starting values are given

    cop = gethac(dim,start_val,type = type) # HAC object with given type and starting values
    cop_est = HAC::estimate.copula(sample, method = 2, margins = "edf", hac = cop, type = type) # Perform pseudo-maximum likelihood estimation

  } else{

    colnames(sample) = NULL
    n = nrow(sample)
    start_val = integer(length(dim))
    start = 1 # Index corresponding to first position of current random vector

    for(i in 1:length(dim)){ # Starting values for child copulas are obtained from MLE corresponding to marginal samples

      sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector

      if(dim[i] == 1){

        start_val[i] = 1 # Does not matter when child is one dimensional

      } else{

        # Starting values for marginal MLE procedures is 2

        start_val[i] = HAC::estimate.copula(sample[,start:sumdim], method = 2, margins = "edf",
                                            hac = gethac(rep(1,dim[i]),rep(2,dim[i]+1),type = type),type = type)$tree[[dim[i]+1]]

      }

      start = sumdim + 1 # Update index

    }

    start_val = pmin(start_val,25) # Upper bound for starting values
    start_val = pmax(start_val,2) # Lower bound for starting values
    cop = gethac(dim,c(1.9,start_val),type = type) # Starting value for theta_0 is 1.9
    cop_est = HAC::estimate.copula(sample, method = 2, margins = "edf", hac = cop, type = type)

  }

  k = length(cop_est$tree) - 1 # Number of random vectors

  # It might be possible that full likelihood returns 2 possible parameter values,
  # we then return the first solution

  thetas_valid = integer(k)
  dim_new = integer(k) # estimate.copula might change order of dim

  for(i in 1:k){

    if(length(cop_est$tree[[i]]) > 1){
      dim_new[i] = length(cop_est$tree[[i]]) - 1
      thetas_valid[i] = cop_est$tree[[i]][[dim_new[i]+1]][1]

    } else{

      dim_new[i] = 1

    }
  }

  # We now make sure that thetas_valid corresponds to the original dim again

  thetas_valid = thetas_valid[thetas_valid != 0]
  thetas = integer(k)
  pos = 1

  for(i in 1:k){

    if(dim[i] == 1){

      thetas[i] = 1

    } else{

      thetas[i] = thetas_valid[pos]
      pos = pos + 1

    }
  }

  thetas = c(cop_est$tree[[k+1]],thetas)
  cop_est = gethac(dim,thetas,type = type)

  return(cop_est)

}
