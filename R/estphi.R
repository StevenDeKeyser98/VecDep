#' @title estphi
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function estimates the \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} by estimating the joint and marginal
#' copula densities.
#'
#' @param sample A sample from a \eqn{q}-dimensional random vector \eqn{\mathbf{X}} (\eqn{n \times q} matrix with observations in rows, variables in columns).
#' @param dim  The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#' @param est_method The method used for estimating the \eqn{\Phi}-dependence.
#' @param phi The function \eqn{\Phi}.
#'
#' @details
#' When \eqn{\mathbf{X}} has copula density \eqn{c} with marginal copula densities \eqn{c_{i}} of \eqn{\mathbf{X}_{i}} for \eqn{i = 1, \dots, k},
#' the \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}} equals
#' \deqn{\mathcal{D}_{\Phi} \left (\mathbf{X}_{1}, \dots, \mathbf{X}_{k} \right ) = \mathbb{E} \left \{ \frac{\prod_{i = 1}^{k} c_{i}(\mathbf{U}_{i})}{c \left ( \mathbf{U} \right )} \Phi \left (\frac{c(\mathbf{U})}{\prod_{i = 1}^{k}c_{i}(\mathbf{U}_{i})} \right ) \right \},}
#' for a certain continuous, convex function \eqn{\Phi : (0,\infty) \rightarrow \mathbb{R}}, and with \eqn{\mathbf{U} = (\mathbf{U}_{1}, \dots, \mathbf{U}_{k}) \sim c}.
#'
#' This functions allows to estimate \eqn{\mathcal{D}_{\Phi}} in several ways (options for est_method)
#'
#' \itemize{ \item list("hac", type = type, M = M) for parametric estimation by fitting a hierarchical Archimedean copula (hac) via pseudo-maximum likelihood estimation,
#'       using a generator of type = type and a simulated Monte Carlo sample of size \eqn{M} in order to approximate the expectation, see also the functions \code{\link{mlehac}} and \code{\link{phihac}},
#'       \item list("nphac", estimator = estimator, type = type) for fully non-parametric estimation using the beta kernel estimator
#'       or Gaussian transformation kernel estimator using a fitted hac (via pseudo-maximum likelihood estimation) of type = type to find locally optimal bandwidths, see also the function \code{\link{phinp}},
#'       \item list("np", estimator = estimator, bw_method = bw_method) for fully non-parametric estimation using the beta kernel estimator or
#'              Gaussian transformation kernel estimator, see \code{\link{phinp}} for different bw_method arguments (either 1 or 2, for performing local bandwidth selection),
#'       \item list("ellip", grid = grid) for semi-parametric estimation through meta-elliptical copulas, with bandwidths determined by the \code{\link{elliptselect}} function,
#'       see also the function \code{\link{phiellip}}.}
#'
#'
#' @return The estimated \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}}.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' Hierarchical variable clustering via copula-based divergence measures between random vectors.
#' International Journal of Approximate Reasoning 165:109090.
#' doi: https://doi.org/10.1016/j.ijar.2023.109090.
#'
#' De Keyser, S. & Gijbels, I. (2024).
#' Parametric dependence between random vectors via copula-based divergence measures.
#' Journal of Multivariate Analysis 203:105336. \cr
#' doi: https://doi.org/10.1016/j.jmva.2024.105336.
#'
#' @seealso \code{\link{phihac}} for computing the \eqn{\Phi}-dependence between all the child copulas of a hac object with two nesting levels,
#'          \code{\link{phinp}} for fully non-parametric estimation of the \eqn{\Phi}-dependence between \eqn{k} random vectors,
#'          \code{\link{phiellip}} for estimating the \eqn{\Phi}-dependence between \eqn{k} random vectors having a meta-elliptical copula.
#'
#'
#' @examples
#' \donttest{
#'
#' # Hierarchical Archimedean copula setting
#' q = 4
#' dim = c(2,2)
#'
#' # Sample size
#' n = 1000
#'
#' # Four dimensional hierarchical Gumbel copula
#' # with parameters (theta_0,theta_1,theta_2) = (2,3,4)
#' hac = gethac(dim,c(2,3,4),type = 1)
#'
#' # Sample
#' sample = HAC::rHAC(n,hac)
#'
#' # Several estimators for the mutual information between two random vectors of size 2
#'
#' est_phi_1 = estphi(sample,dim,list("hac",type = 1,M = 10000),function(t){t * log(t)})
#' est_phi_2 = estphi(sample,dim,list("nphac",estimator = "beta",type = 1),
#'                                    function(t){t * log(t)})
#' est_phi_3 = estphi(sample,dim,list("nphac",estimator = "trans",type = 1),
#'                                    function(t){t * log(t)})
#' est_phi_4 = estphi(sample,dim,list("np",estimator = "beta",bw_method = 1),
#'                                    function(t){t * log(t)})
#' est_phi_5 = estphi(sample,dim,list("np",estimator = "trans",bw_method = 1),
#'                                    function(t){t * log(t)})
#' est_phi_6 = estphi(sample,dim,list("np",estimator = "beta",bw_method = 2),
#'                                    function(t){t * log(t)})
#' est_phi_7 = estphi(sample,dim,list("np",estimator = "trans",bw_method = 2),
#'                                    function(t){t * log(t)})
#'
#' true_phi = phihac(hac,dim,10000,function(t){t * log(t)})
#'
#' # Gaussian copula setting
#'
#' q = 4
#' dim = c(2,2)
#'
#' # Sample size
#' n = 1000
#'
#' # AR(1) correlation matrix with correlation 0.5
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1)))
#'
#' # Sample from 4-dimensional normal distribution
#' sample = mvtnorm::rmvnorm(n,rep(0,q),R,method = "chol")
#'
#' # Estimate mutual information via MECIP procedure
#' est_phi = estphi(sample,dim,list("ellip",grid = seq(0.005,100,by = 0.005)),
#'                                  function(t){t * log(t)})
#'
#' true_phi = minormal(R,dim)
#'}
#'
#' @export

estphi = function(sample, dim, est_method, phi){

  if(est_method[[1]] == "hac"){ # Use hierarchical Archimedean copula

    cop_est = mlehac(sample,dim,est_method$type) # Fit hac copula object via maximum pseudo-likelihood estimation

    if(ncol(sample) == 2){ # Make negative dependence parameter positive in case of 2D copula

      cop_est$tree[[3]] = abs(cop_est$tree[[3]])

    }

    return(phihac(cop = cop_est,dim = dim,M = est_method$M,phi = phi)) # Compute dependence via Monte Carlo approximation

  }

  if(est_method[[1]] == "nphac"){ # Kernel copula density estimation with fitted hac as reference copula for bandwidths

    cop_est = mlehac(sample,dim,est_method$type) # Fit hac copula model via MLE

    if(ncol(sample) == 2){ # Make negative dependence coefficient positive

      cop_est$tree[[3]] = abs(cop_est$tree[[3]])

    }

    return(phinp(sample = sample,cop = cop_est,dim = dim,phi = phi,estimator = est_method$estimator,bw_method = 0)) # Compute dependence via kernel estimator

  }

  if(est_method[[1]] == "np"){ # Kernel copula density estimation with kernel estimator as reference copula for bandwidths

    return(phinp(sample = sample,cop = NULL,dim = dim,phi = phi,estimator = est_method$estimator,bw_method = est_method$bw_method))

  }

  if(est_method[[1]] == "ellip"){ # Use improved MECIP for estimating the dependence

    grid = est_method$grid

    # Parameter selection

    n = nrow(sample) ; kn = length(dim[which(dim != 1)]) ; q = sum(dim)
    params = list("h" = integer(kn+1), "a" = integer(kn+1), "p" = integer(kn+1), "r" = integer(kn+1))
    joint_select = elliptselect(n,q,seq((3/4)-(1/q)+0.01,1-0.01,len = 200),seq(0.01,2,len = 200))
    params$h[1] = joint_select$Opth ; params$a[1] = joint_select$Opta ; params$p[1] = joint_select$Optp

    step = 2

    for(i in 1:length(dim)){

      if(dim[i] != 1){

        marg_select = elliptselect(n,dim[i],seq((3/4)-(1/dim[i])+0.01,1-0.01,len = 200),seq(0.01,2,len = 200))
        params$h[step] = marg_select$Opth ; params$a[step] = marg_select$Opta ; params$p[step] = marg_select$Optp
        step = step + 1

      }
    }

    return(phiellip(sample = sample,dim = dim,phi = phi,grid = grid,params = params))

  }
}

