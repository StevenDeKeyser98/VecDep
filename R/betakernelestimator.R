#' @title betakernelestimator
#'
#' @description This function computes the non-parametric beta kernel copula density estimator.
#'
#' @param input The copula argument at which the density estimate is to be computed.
#' @param h The bandwidth to be used in the beta kernel.
#' @param  pseudos The (estimated) copula observations from a \eqn{q}-dimensional random vector \eqn{\mathbf{X}} (\eqn{n \times q} matrix with observations in rows, variables in columns).
#'
#' @details
#' Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1}, \dots, \mathbf{X}_{k})} with \eqn{\mathbf{X}_{i} = (X_{i1}, \dots, X_{id_{i}})},
#' and samples \eqn{X_{ij}^{(1)}, \dots, X_{ij}^{(n)}} from \eqn{X_{ij}} for \eqn{i = 1, \dots, k} and \eqn{j = 1, \dots, d_{i}},
#' the beta kernel estimator for the copula density of \eqn{\mathbf{X}} equals, at \eqn{\mathbf{u} = (u_{11}, \dots, u_{kd_{k}}) \in \mathbb{R}^{q}},
#' \deqn{\widehat{c}_{\text{B}}(\mathbf{u}) = \frac{1}{n} \sum_{\ell = 1}^{n} \prod_{i = 1}^{k} \prod_{j = 1}^{d_{i}} k_{\text{B}} \left (\widehat{U}_{ij}^{(\ell)},\frac{u_{ij}}{h_{n}} + 1, \frac{1-u_{ij}}{h_{n}} + 1 \right ),}
#' where \eqn{h_{n} > 0} is a bandwidth parameter, \eqn{\widehat{U}_{ij}^{(\ell)} = \widehat{F}_{ij} (X_{ij}^{(\ell)})} with \eqn{\widehat{F}_{ij}(x_{ij}) = \frac{1}{n+1} \sum_{\ell = 1}^{n} 1 \left (X_{ij}^{(\ell)} \leq x_{ij} \right )} the (rescaled) empirical cdf of \eqn{X_{ij}}, and
#' \deqn{k_{\text{B}}(u,\alpha,\beta) = \frac{u^{\alpha - 1} (1-u)^{\beta-1}}{B(\alpha,\beta)},} with \eqn{B} the beta function.
#'
#'
#' @return The beta kernel copula density estimator evaluated at the input.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' Hierarchical variable clustering via copula-based divergence measures between random vectors.
#' International Journal of Approximate Reasoning 165:109090.
#' doi: https://doi.org/10.1016/j.ijar.2023.109090.
#'
#' @seealso \code{\link{transformationestimator}} for the computation of the Gaussian transformation kernel copula density estimator,
#'          \code{\link{hamse}} for local bandwidth selection for the beta kernel or Gaussian transformation kernel copula density estimator,
#'          \code{\link{phinp}} for fully non-parametric estimation of the \eqn{\Phi}-dependence between \eqn{k} random vectors.
#'
#' @examples
#' q = 3
#' n = 100
#'
#' sample = mvtnorm::rmvnorm(n,rep(0,q),diag(3),method = "chol") # Sample from multivariate normal distribution with identity covariance matrix
#' pseudos = matrix(0,n,q)
#' for(j in 1:q){pseudos[,j] = (n/(n+1)) * ecdf(sample[,j])(sample[,j])} # Copula pseudo-observations
#'
#' input = rep(0.5,q) # Argument at which to estimate the density
#' h = hamse(input,pseudos = pseudos,n = n,estimator = "beta",bw_method = 1) # Local bandwidth selection
#' est_dens = betakernelestimator(input,h,pseudos) # Beta kernel estimator
#' true = copula::dCopula(input, copula::normalCopula(0, dim = q)) # True density
#'
#' @export

betakernelestimator = function(input, h, pseudos){

  n = nrow(pseudos) # Sample size
  q = length(input) # Total dimension

  sum = 0

  for(j in 1:n){

    prod = 1

    for(i in 1:q){

      prod = prod * dbeta(pseudos[j,i],(input[i]/h)+1,((1-input[i])/h)+1)

    }

    sum = sum + prod

  }

  return((1/n)*sum)

}

