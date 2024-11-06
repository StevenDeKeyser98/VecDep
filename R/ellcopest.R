#' @title ellcopest
#'
#' @description This functions performs improved kernel density estimation of the generator of a meta-elliptical
#' copula by using Liebscher's algorithm, combined with a shrinkage function.
#'
#' @param dataU The (estimated) copula observations from a \eqn{q}-dimensional random vector \eqn{\mathbf{X}} (\eqn{n \times q} matrix with observations in rows, variables in columns).
#' @param Sigma_m1 The (estimated) inverse of the scale matrix of the meta-elliptical copula.
#' @param h The bandwidth of the kernel.
#' @param grid The grid of values on which to estimate the density generator.
#' @param niter The number of iterations used in the MECIP (default = 10).
#' @param a The tuning parameter to improve the performance at \eqn{0}.
#' @param Kernel The kernel used for the smoothing (default = "epanechnikov").
#' @param shrink The shrinkage function to further improve the performance at \eqn{0} and guarantee the existence of the AMISE bandwidth.
#' @param verbose See the EllDistrEst function of the R package ElliptCopulas.
#' @param startPoint See the EllDistrEst function of the R package ElliptCopulas.
#' @param prenormalization See the EllDistrEst function of the R package ElliptCopulas.
#' @param normalize  A value in \eqn{\{1,2\}} indicating the normalization procedure that is applied to the estimated generator (default = 1).
#'
#' @details
#' The context is the one of a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1}, \dots, \mathbf{X}_{k})},
#' with \eqn{\mathbf{X}_{i} = (X_{i1}, \dots, X_{id_{i}})} for \eqn{i = 1, \dots, k}, having a meta-elliptical copula.
#' This means that there exists a generator \eqn{g_{\mathcal{R}} : (0,\infty) \rightarrow \mathbb{R}} and a quantile function \eqn{Q}, such that the random vector \eqn{\mathbf{Z} = (\mathbf{Z}_{1}, \dots, \mathbf{Z}_{k})} with
#' \deqn{\mathbf{Z}_{i} = (Z_{i1}, \dots, Z_{id_{i}}) = \left(\left (Q \circ F_{i1} \right ) \left (X_{i1} \right ), \dots, \left (Q \circ F_{id_{i}} \right ) \left (X_{id_{i}} \right )  \right )} for \eqn{i = 1, \dots, k},
#' where \eqn{F_{ij}} is the cdf of \eqn{X_{ij}}, has a multivariate elliptical distribution.
#' Denoting \eqn{\widehat{F}_{ij}(x_{ij}) = \frac{1}{n+1} \sum_{\ell = 1}^{n} 1 \left (X_{ij}^{(\ell)} \leq x_{ij} \right )} for the (rescaled) empirical cdf of \eqn{X_{ij}} based on a sample \eqn{X_{ij}^{(1)}, \dots, X_{ij}^{(n)}} for \eqn{i = 1, \dots, k} and \eqn{j = 1, \dots, d_{i}},
#' and \eqn{\widehat{\mathbf{R}}} for an estimator of the scale matrix \eqn{\mathbf{R}}, this functions estimates \eqn{g_{\mathcal{R}}} by using the MECIP (Meta-Elliptical Copula Iterative Procedure) of Derumigny & Fermanian (2022).
#'
#' This means that we start from an initial guess \eqn{\widehat{g}_{\mathcal{R},0}} for the generator \eqn{g_{\mathcal{R}}}, based on which we obtain an estimated
#' sample from \eqn{\mathbf{Z}} through the quantile function corresponding to \eqn{\widehat{g}_{\mathcal{R},0}}.
#' Based on this estimated sample, we then obtain an estimator \eqn{\widehat{g}_{\mathcal{R},1}} using the function
#' \code{\link{elldistrest}}, performing improved kernel estimation with shrinkage function.
#' This procedure is repeated for a certain amount (niter) of iterations to obtain a final estimate for \eqn{g_{\mathcal{R}}}.
#'
#' The estimator without the shrinkage function \eqn{\alpha} is implemented in the R package 'ElliptCopulas'.
#' We use this implementation and bring in the shrinkage function.
#'
#' In order to make \eqn{g_{\mathcal{R}}} identifiable, an extra normalization procedure is implemented
#' in line with an extra constraint on \eqn{g_{\mathcal{R}}}.
#' When normalize = 1, this corresponds to \eqn{\mathbf{R}} being the correlation matrix of \eqn{\mathbf{Z}}.
#' When normalize = 2, this corresponds to the identifiability condition of Derumigny & Fermanian (2022).
#'
#' @return The estimates for \eqn{g_{\mathcal{R}}} at the grid points.
#'
#' @references
#' Derumigny, A., Fermanian, J.-D., Ryan, V., van der Spek, R. (2024).
#' ElliptCopulas, R package version 0.1.4.1.
#' url: https://CRAN.R-project.org/package=ElliptCopulas.
#'
#' Derumigny, A. & Fermanian, J.-D. (2022).
#' Identifiability and estimation of meta-elliptical copula generators.
#' Journal of Multivariate Analysis 190:104962.
#'
#' doi: https://doi.org/10.1016/j.jmva.2022.104962.
#'
#' De Keyser, S. & Gijbels, I. (2024).
#' Hierarchical variable clustering via copula-based divergence measures between random vectors.
#' International Journal of Approximate Reasoning 165:109090.
#' doi: https://doi.org/10.1016/j.ijar.2023.109090.
#'
#' Liebscher, E. (2005).
#' A semiparametric density estimator based on elliptical distributions.
#' Journal of Multivariate Analysis 92(1):205-225.
#' doi: https://doi.org/10.1016/j.jmva.2003.09.007.
#'
#' @seealso \code{\link{elldistrest}} for improved kernel estimation of the elliptical generator of an elliptical distribution,
#'          \code{\link{elliptselect}} for selecting optimal tuning parameters for the improved kernel estimator of the elliptical generator,
#'          \code{\link{phiellip}} for estimating the \eqn{\Phi}-dependence between \eqn{k} random vectors having a meta-elliptical copula.
#'
#' @examples
#' \dontrun{
#' q = 4
#'
#' # Sample size
#' n = 1000
#'
#' # Grid on which to evaluate the elliptical generator
#' grid = seq(0.005,100,by = 0.005)
#'
#' # Degrees of freedom
#' nu = 7
#'
#' # Student-t generator with 7 degrees of freedom
#' g_q = ((nu/(nu-2))^(q/2))*(gamma((q+nu)/2)/(((pi*nu)^(q/2))*gamma(nu/2))) *
#'       ((1+(grid/(nu-2)))^(-(q+nu)/2))
#'
#' # Density of squared radius
#' R2 = function(t,q){(gamma((q+nu)/2)/(((nu-2)^(q/2))*gamma(nu/2)*gamma(q/2))) *
#'                    (t^((q/2)-1)) * ((1+(t/(nu-2)))^(-(q+nu)/2))}
#'
#' # Sample from 4-dimensional Student-t distribution with 7 degrees of freedom
#' # and identity covariance matrix
#' sample = ElliptCopulas::EllDistrSim(n,q,diag(q),density_R2 = function(t){R2(t,q)})
#'
#' # Copula pseudo-observations
#' pseudos = matrix(0,n,q)
#' for(j in 1:q){pseudos[,j] = (n/(n+1)) * ecdf(sample[,j])(sample[,j])}
#'
#' # Shrinkage function
#' shrinkage = function(t,p){1-(1/((t^p) + 1))}
#'
#' # Tuning parameter selection
#' opt_parameters = elliptselect(n,q,seq((3/4)-(1/q)+0.01,1-0.01,len = 200),
#'                                   seq(0.01,2,len = 200))
#'
#' # Optimal tuning parameters
#' a = opt_parameters$Opta ; p = opt_parameters$Optp ; h = opt_parameters$Opth
#'
#' # Estimated elliptical generator
#' g_est = ellcopest(dataU = pseudos,Sigma_m1 = diag(q),h = h,grid = grid,a = a,
#'                   shrink = function(t){shrinkage(t,p)})
#'
#' plot(grid,g_est,type = "l", xlim = c(0,8))
#' lines(grid,g_q,col = "green")
#'}
#'
#' @export

ellcopest = function(dataU, Sigma_m1, h, grid, niter = 10, a, Kernel = "epanechnikov", shrink,
                     verbose = 1, startPoint = "identity", prenormalization = FALSE, normalize = 1){

  # See R package ElliptCopulas (in particular the EllCopEst function for more details on the code)
  # The difference is that we also use the shrinkage function here, and allow for another normalization (normalize = 1)

  dataUisNA = is.na(dataU)

  if(any(c(dataU[!dataUisNA] <= 0, dataU[!dataUisNA] >= 1))){
    stop("Values of dataU should be strictly between 0 and 1.")
  }

  whichRowsHasNA = which(apply(X = dataU, MARGIN = 1, anyNA))

  if (length(whichRowsHasNA) > 0){
    Sigma = solve(Sigma_m1)
  }

  d = length(dataU[1,])
  list_path_gdh = list()

  initializationStandard(env = environment(),shrink = shrink, normalize = normalize)
  list_path_gdh[[1]] = environment()[["g_d_norm"]]

  i_iter = 1
  if (verbose > 0) {cat("iteration: ") ; cat(i_iter) ; cat("\n") }
  iterationStandard(env = environment(),shrink = shrink, normalize = normalize)
  list_path_gdh[[2]] = environment()[["g_d_norm"]]

  i_iter = 2
  doContinueIter = TRUE

  while(doContinueIter)

  {
    if (verbose > 0) {cat("iteration: ") ; cat(i_iter) ; cat("\n") }

    iterationStandard(env = environment(),shrink = shrink, normalize = normalize)
    list_path_gdh[[i_iter+1]] <- environment()[["g_d_norm"]]
    doContinueIter = (i_iter < niter)
    i_iter = i_iter + 1
  }

  return(environment()[["g_d_norm"]])

}

# Auxiliary functions

initializationStandard = function(env,shrink = shrink,normalize = normalize){

  # Function from package ElliptCopulas, but with EllDistrEst replaced by elldistrest

  if(env$startPoint == "gaussian"){

    env$g_d = exp(-env$grid/2)

  } else if(env$startPoint == "identity"){

    env$Qg1 = function(u){return(u)}

    dataZ = apply(X = env$dataU, FUN = env$Qg1, MARGIN = c(1,2))

    if (length(env$whichRowsHasNA) > 0){

      dataZ = simulateEllDistrForNA(
        dataZ = dataZ, grid = env$grid, d = env$d,
        Sigma = env$Sigma, whichRowsHasNA = env$whichRowsHasNA, g_d = exp( - env$grid/2),
        genR = env$genR)
    }

    env$g_d = elldistrest(
      Z = dataZ, mu = 0, Sigma_m1 = env$Sigma_m1, shrink = shrink,
      grid = env$grid, h = env$h, Kernel = env$Kernel, a = env$a, normalize = FALSE)

  } else if (env$startPoint =="A~Phi^{-1}"){

    env$Qg1 = function(u){return(stats::qnorm(u))}

    dataZ = apply(X = env$dataU, FUN = env$Qg1, MARGIN = c(1,2))

    if(length(env$whichRowsHasNA) > 0){

      dataZ = simulateEllDistrForNA(
        dataZ = dataZ, grid = env$grid, d = env$d,
        Sigma = env$Sigma, whichRowsHasNA = env$whichRowsHasNA, g_d = exp( - env$grid/2),
        genR = env$genR)
    }

    env$g_d = elldistrest(Z = dataZ, mu = 0, Sigma_m1 = env$Sigma_m1, shrink = shrink,
                          grid = env$grid, h = env$h, Kernel = env$Kernel, a = env$a, normalize = FALSE)

  } else{stop("Wrong startPoint. Possible choices are 'gaussian', 'identity' and 'A~Phi^{-1}' .")}

  if(normalize == 1){ # Normalize generator such that scale matrix is the covariance matrix

    env$g_d_norm = Densitygenerator.normalize(
      grid_g = env$g_d, grid = env$grid, d = env$d, verbose = (env$verbose - 1))

  } else if(normalize == 2){ # Normalize according to ElliptCopulas package

    env$g_d_norm = ElliptCopulas::DensityGenerator.normalize(
      grid_g = env$g_d, grid = env$grid, d = env$d, verbose = (env$verbose - 1))

  }
}

iterationStandard = function(env,shrink = shrink,normalize = normalize){

  # Function from package ElliptCopulas

  g_1 = ElliptCopulas::Convert_gd_To_g1(grid = env$grid , g_d = env$g_d_norm , d = env$d)
  Qg1 = ElliptCopulas::Convert_g1_To_Qg1(grid = env$grid , g_1 = g_1)
  dataZ <- apply(X = env$dataU, FUN = Qg1, MARGIN = c(1,2))

  if(length(env$whichRowsHasNA) > 0){

    dataZ = simulateEllDistrForNA(
      dataZ = dataZ, grid = env$grid, d = env$d,
      Sigma = env$Sigma, whichRowsHasNA = env$whichRowsHasNA, g_d = env$g_d_norm,
      genR = env$genR)
  }

  if (env$prenormalization){

    varZ = stats::var(x = as.numeric(dataZ))
    env$g_d = elldistrest(
      Z = dataZ, mu = 0, Sigma_m1 = env$Sigma_m1 / varZ, shrink = shrink,
      grid = env$grid, h = env$h, Kernel = env$Kernel, a = env$a, normalize = FALSE)

  } else {

    env$g_d = elldistrest(
      Z = dataZ, mu = 0, Sigma_m1 = env$Sigma_m1, shrink = shrink,
      grid = env$grid, h = env$h, Kernel = env$Kernel, a = env$a, normalize = FALSE)
  }

  if(normalize == 1){ # Normalize generator such that scale matrix is the covariance matrix

    env$g_d_norm = Densitygenerator.normalize(
      grid_g = env$g_d, grid = env$grid, d = env$d, verbose = (env$verbose > 1))

  } else if(normalize == 2){ # Normalize according to ElliptCopulas package

    env$g_d_norm = ElliptCopulas::DensityGenerator.normalize(
      grid_g = env$g_d, grid = env$grid, d = env$d, verbose = (env$verbose > 1))

  }
}
