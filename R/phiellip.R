#' @title phiellip
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function estimates the \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} by estimating the joint and marginal
#' meta-elliptical copula generators via the MECIP.
#'
#' @param sample A sample from a \eqn{q}-dimensional random vector \eqn{\mathbf{X}} (\eqn{n \times q} matrix with observations in rows, variables in columns).
#' @param dim The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#' @param phi The function \eqn{\Phi}.
#' @param grid The grid of values on which to estimate the density generators.
#' @param params The tuning parameters to be used when estimating the density generators.
#' @param normalize A value in \eqn{\{1,2\}} indicating the normalization procedure that is applied to the estimated generator (default = 1).
#'
#' @details
#' When \eqn{\mathbf{X} = (\mathbf{X}_{1}, \dots, \mathbf{X}_{k})} has a meta-elliptical copula with generator \eqn{g_{\mathcal{R}}}, marginal generators \eqn{g_{\mathcal{R}_{i}}} of \eqn{\mathbf{X}_{i}} for \eqn{i = 1, \dots, k}, and scale matrix \deqn{\mathbf{R} = \begin{pmatrix} \mathbf{R}_{11} & \mathbf{R}_{12} & \cdots & \mathbf{R}_{1k} \\
#'                                                              \mathbf{R}_{12}^{\text{T}} & \mathbf{R}_{22} & \cdots & \mathbf{R}_{2k} \\
#'                                                              \vdots & \vdots & \ddots & \vdots \\
#'                                                              \mathbf{R}_{1k}^{\text{T}} & \mathbf{R}_{2k}^{\text{T}} & \cdots & \mathbf{R}_{kk} \end{pmatrix},} the \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}} equals
#' \deqn{\mathcal{D}_{\Phi}\left (\mathbf{X}_{1}, \dots, \mathbf{X}_{k} \right ) = \mathbb{E} \left \{\frac{\prod_{i = 1}^{k} g_{\mathcal{R}_{i}}\left (\mathbf{Z}_{i}^{\text{T}} \mathbf{R}_{ii}^{-1} \mathbf{Z}_{i} \right ) \left | \mathbf{R} \right |^{1/2}}{g_{\mathcal{R}}\left (\mathbf{Z}^{\text{T}} \mathbf{R}^{-1} \mathbf{Z} \right ) \prod_{i = 1}^{k} \left | \mathbf{R}_{ii} \right |^{1/2}} \Phi \left (\frac{g_{\mathcal{R}} \left (\mathbf{Z}^{\text{T}} \mathbf{R}^{-1} \mathbf{Z} \right ) \prod_{i = 1}^{k} \left |\mathbf{R}_{ii} \right |^{1/2}}{\prod_{i = 1}^{k} g_{\mathcal{R}_{i}} \left (\mathbf{Z}_{i}^{\text{T}}\mathbf{R}_{ii}^{-1} \mathbf{Z}_{i} \right ) \left |\mathbf{R} \right |^{1/2} } \right )\right \},}
#' where (recall that \eqn{\mathbf{X}_{i} = (X_{i1}, \dots, X_{id_{i}})} for \eqn{i = 1, \dots, k})
#' \deqn{\mathbf{Z}_{i} = (Z_{i1}, \dots, Z_{id_{i}}) = \left(\left (Q \circ F_{i1} \right ) \left (X_{i1} \right ), \dots, \left (Q \circ F_{id_{i}} \right ) \left (X_{id_{i}} \right )  \right ),}
#' and \eqn{\mathbf{Z} = (\mathbf{Z}_{1}, \dots, \mathbf{Z}_{k})}, with \eqn{Q} the quantile function corresponding to \eqn{g_{\mathcal{R}}}.
#'
#' The expectation \eqn{\mathbb{E}} is replaced by the empirical mean using the estimated sample \eqn{\widehat{\mathbf{Z}}^{(1)}, \dots, \widehat{\mathbf{Z}}^{(n)}} with \eqn{\widehat{\mathbf{Z}}^{(\ell)} = (\widehat{\mathbf{Z}}_{1}^{(\ell)}, \dots, \widehat{\mathbf{Z}}_{k}^{(\ell)})} for \eqn{\ell = 1, \dots, n}, where
#' \deqn{\widehat{\mathbf{Z}}_{i}^{(\ell)} = \left (\widehat{Z}_{i1}^{(\ell)}, \dots, \widehat{Z}_{id_{i}}^{(\ell)} \right ) = \left ( \left (\widehat{Q} \circ \widehat{F}_{i1} \right ) \left (X_{i1}^{(\ell)} \right ), \dots, \left (\widehat{Q} \circ \widehat{F}_{id_{i}} \right ) \left (X_{id_{i}}^{(\ell)} \right ) \right ),} for \eqn{i = 1, \dots, k}.
#' Here, \eqn{\widehat{Q}} will be the quantile function corresponding to the final estimator for \eqn{g_{\mathcal{R}}}, and \eqn{\widehat{F}_{ij}(x_{ij}) = \frac{1}{n+1} \sum_{\ell = 1}^{n} 1 \left (X_{ij}^{(\ell)} \leq x_{ij} \right )} is the (rescaled) empirical cdf of \eqn{X_{ij}} based on a sample \eqn{X_{ij}^{(1)}, \dots, X_{ij}^{(n)}} for \eqn{i = 1, \dots, k} and \eqn{j = 1, \dots, d_{i}}.
#'
#' The estimation of \eqn{\mathbf{R}} is done via its relation with the Kendall's tau matrix, see the function ElliptCopulas::KTMatrixEst in
#' the R package 'ElliptCopulas' of Derumigny et al. (2024).
#'
#' For estimating \eqn{g_{\mathcal{R}}} and \eqn{g_{\mathcal{R}_{i}}} for \eqn{i = 1, \dots, k}, the function \code{\link{ellcopest}} is used.
#' This function requires certain tuning parameters (a bandwidth \eqn{h}, a parameter \eqn{a}, and a parameter \eqn{\delta} for the shrinkage function). Suppose that there are
#' \eqn{m} marginal random vectors (among \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}}) that are of dimension strictly larger than one.
#' Then, all tuning parameters should be given as
#' \deqn{\text{params} = \text{list}(\text{``h"} = (h,h_{1},\dots,h_{m}), \text{``a"} = (a,a_{1}, \dots, a_{m}), \text{``p"} = (\delta, \delta_{1}, \dots, \delta_{m})),}
#' i.e., \eqn{(h,a,\delta)} will be used for estimating \eqn{g_{\mathcal{R}}}, and \eqn{(h_{i},a_{i},\delta_{i})} will be used for estimating \eqn{g_{\mathcal{R}_{i}}} for \eqn{i = 1, \dots, k}.
#'
#' When \eqn{d_{i} = 1} for a certain \eqn{i \in \{1, \dots, k \}}, the function Convert_gd_To_g1 from the R package 'ElliptCopulas' is used to estimate \eqn{g_{\mathcal{R}_{i}}}.
#'
#' In order to make \eqn{g_{\mathcal{R}}} identifiable, an extra normalization procedure is implemented
#' in line with an extra constraint on \eqn{g_{\mathcal{R}}}.
#' When normalize = 1, this corresponds to \eqn{\mathbf{R}} being the correlation matrix of \eqn{\mathbf{Z}}.
#' When normalize = 2, this corresponds to the identifiability condition of Derumigny & Fermanian (2022).
#'
#' @return The estimated \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}}.
#'
#' @references
#' Derumigny, A., Fermanian, J.-D., Ryan, V., van der Spek, R. (2024).
#' ElliptCopulas, R package version 0.1.4.1.
#' url: https://CRAN.R-project.org/package=ElliptCopulas.
#'
#' De Keyser, S. & Gijbels, I. (2024).
#' Hierarchical variable clustering via copula-based divergence measures between random vectors.
#' International Journal of Approximate Reasoning 165:109090.
#' doi: https://doi.org/10.1016/j.ijar.2023.109090.
#'
#'
#' @seealso \code{\link{elldistrest}} for improved kernel estimation of the elliptical generator of an elliptical distribution,
#'          \code{\link{ellcopest}} for improved kernel estimation of the elliptical generator of a meta-elliptical copula,
#'          \code{\link{elliptselect}} for selecting optimal tuning parameters for the improved kernel estimator of the elliptical generator.
#'
#' @examples
#' q = 4
#' dim = c(2,2)
#' n = 1000 # Sample size
#' grid = seq(0.005,100,by = 0.005) # Grid on which to evaluate the elliptical generator
#' nu = 7 # Degrees of freedom
#' g_q = ((nu/(nu-2))^(q/2))*(gamma((q+nu)/2)/(((pi*nu)^(q/2))*gamma(nu/2))) * ((1+(grid/(nu-2)))^(-(q+nu)/2)) # Student-t generator with 7 degrees of freedom
#' R2 = function(t,q){(gamma((q+nu)/2)/(((nu-2)^(q/2))*gamma(nu/2)*gamma(q/2))) * (t^((q/2)-1)) * ((1+(t/(nu-2)))^(-(q+nu)/2))} # Density of squared radius
#' sample = ElliptCopulas::EllDistrSim(n,q,diag(q),density_R2 = function(t){R2(t,q)}) # Sample from 4-dimensional Student-t distribution with 7 degrees of freedom
#' # and identity covariance matrix
#' opt_parameters_joint = elliptselect(n,q,seq((3/4)-(1/q)+0.01,1-0.01,len = 200),seq(0.01,2,len = 200)) # Tuning parameter selection for g_R
#' a = opt_parameters_joint$Opta ; p = opt_parameters_joint$Optp ; h = opt_parameters_joint$Opth  # Optimal tuning parameters for g_R
#' opt_parameters_marg = elliptselect(n,2,seq((3/4)-(1/2)+0.01,1-0.01,len = 200),seq(0.01,2,len = 200)) # Tuning parameter selection for g_R_1 (same for g_R_2)
#' a1 = opt_parameters_marg$Opta ; p1 = opt_parameters_marg$Optp ; h1 = opt_parameters_marg$Opth  # Optimal tuning parameters for g_R_1 (same for g_R_2)
#' a2 = a1 ; p2 = p1 ; h2 = h1
#' params = list("h" = c(h,h1,h2), "a" = c(a,a1,a2), "p" = c(p,p1,p2))
#'
#' est_phi = phiellip(sample, dim, function(t){t * log(t)}, grid, params) # Mutual information between two random vectors of size 2
#'
#' @export

phiellip = function(sample, dim, phi, grid, params, normalize = 1){

  shrinkage = function(t,p){1-(1/((t^p) + 1))}  # Shrinkage function

  n = nrow(sample)
  q = ncol(sample)
  k = length(dim)
  pseudos = matrix(0,n,q)

  for(j in 1:q){pseudos[,j] = (n/(n+1)) * ecdf(sample[,j])(sample[,j])} # Pseudo copula observations

  R_joint = sin((pi/2) * ElliptCopulas::KTMatrixEst(sample)) # Estimate correlation matrix via empirical Kendall's tau matrix
  R_joint_inv = solve(R_joint) # Inverse of estimated correlation matrix

  g_joint = ellcopest(pseudos,R_joint_inv,h = params$h[1],grid = grid,a = params$a[1],
                      shrink = function(t){shrinkage(t,params$p[1])},
                      normalize = normalize) # Estimate joint density generator

  umarg = ElliptCopulas::Convert_gd_To_g1(grid, g_joint, q) # Univariate marginal density generator
  Q = ElliptCopulas::Convert_g1_To_Qg1(grid, g_1 = umarg) # Univariate quantile function
  Z = matrix(0,n,q) # Matrix for Z scores

  for(j in 1:q){Z[,j] = Q(pseudos[,j])} # Z scores

  joint_dens = integer(n)

  for(l in 1:n){

    joint_densTC = as.vector(Z[l,] %*% R_joint_inv %*% Z[l,])
    joint_dens[l] = approx(x = grid, y = g_joint, xout = joint_densTC)$y # Interpolation for joint density in Z[l,]

  }

  joint_dens = joint_dens * det(R_joint)^(-1/2) # Joint density in Z[,l]
  marg_dens = matrix(0,n,k)
  start = 1 # Index corresponding to first position of current random vector
  step = 2 # Index for parameters

  for(i in 1:k){ # Estimate marginal density for each marginal random vector, similarly as above

    sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector

    if(length(start:sumdim) == 1){ # In case of a one dimensional marginal random vector

      marg_dens[,i] = approx(x = grid, y = umarg, xout = Z[,start:sumdim]^2)$y

    } else{

      R_marg = R_joint[start:sumdim,start:sumdim]
      R_marg_inv = solve(R_marg)

      g_marg = ellcopest(pseudos[,start:sumdim],R_marg_inv,h = params$h[step],grid = grid,a = params$a[step],
                         shrink = function(t){shrinkage(t,params$p[step])},
                         normalize = normalize)

      for(l in 1:n){

        marg_densTC = as.vector(Z[l,start:sumdim] %*% R_marg_inv %*% Z[l,start:sumdim])
        marg_dens[l,i] = approx(x = grid, y = g_marg, xout = marg_densTC)$y

      }

      marg_dens[,i] = marg_dens[,i] * det(R_marg)^(-1/2)
      step = step + 1

    }

    start = sumdim + 1 # Update index

  }

  marg_dens = apply(marg_dens, 1, prod) # Product of marginal densities
  dep = (marg_dens/joint_dens) * phi(joint_dens/marg_dens) # Integrand of phi-dependence
  dep = dep[!is.na(dep) & !is.infinite(dep)]

  return(mean(dep))

}


