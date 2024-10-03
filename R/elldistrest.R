#' @title elldistrest
#'
#' @description This functions performs improved kernel density estimation of the generator of an elliptical
#' distribution using Liebscher's algorithm, combined with a shrinkage function.
#'
#' @param Z  A sample from a \eqn{q}-dimensional random vector \eqn{\mathbf{Z}} (\eqn{n \times q} matrix with observations in rows, variables in columns).
#' @param mu The (estimated) mean of \eqn{\mathbf{Z}} (default \eqn{= 0}).
#' @param Sigma_m1 The (estimated) inverse of the scale matrix of \eqn{\mathbf{Z}}.
#' @param grid The grid of values on which to estimate the density generator.
#' @param h The bandwidth of the kernel.
#' @param Kernel The kernel used for the smoothing (default = "epanechnikov").
#' @param a The tuning parameter to improve the performance at \eqn{0}.
#' @param shrink The shrinkage function to further improve the performance at \eqn{0} and guarantee the existence of the AMISE bandwidth.
#' @param normalize  A value in \eqn{\{1,2\}} indicating the normalization procedure that is applied to the estimated generator.
#' @param ... See the R package ElliptCopulas for details on other arguments.

#' @details
#' The context is the one of a \eqn{q}-dimensional random vector \eqn{\mathbf{Z}} following an elliptical distribution with
#' generator \eqn{g_{\mathcal{R}} : (0,\infty) \rightarrow \mathbb{R}} and scale matrix \eqn{\mathbf{R}} such that the density of \eqn{\mathbf{Z}} is given by
#' \deqn{h(\mathbf{z}) = \left |\mathbf{R} \right |^{-1/2} g_{\mathcal{R}} \left (\mathbf{z}^{\text{T}} \mathbf{R}^{-1} \mathbf{z} \right ),}
#' for \eqn{\mathbf{z} \in \mathbb{R}^{q}}. Suppose that a sample \eqn{\mathbf{Z}^{(1)}, \dots, \mathbf{Z}^{(n)}} from \eqn{\mathbf{Z}} is given, and let
#' \eqn{\widehat{\mathbf{R}}} be an estimator for the scale matrix \eqn{\mathbf{R}}. Then, when defining
#' \deqn{\widehat{\mathbf{Y}}^{(\ell)} = \widehat{\mathbf{R}}^{-1/2} \mathbf{Z}^{(\ell)}} for \eqn{\ell = 1, \dots, n},
#' this function computes the estimator \eqn{\widehat{g}_{\mathcal{R}}^{\text{I}}} for \eqn{g_{\mathcal{R}}} given by
#' \deqn{\widehat{g}_{\mathcal{R}}^{\text{I}}(t) = c^{\text{I}}(t) \sum_{\ell = 1}^{n} \left \{k \left (\frac{\psi(t) - \psi \left (\left | \left |\widehat{\mathbf{Y}}^{(\ell)} \right | \right |^{2} \right )}{h_{n} \alpha \left (\psi(t) \right )} \right ) + k \left (\frac{\psi(t) + \psi \left (\left | \left |\widehat{\mathbf{Y}}^{(\ell)} \right | \right |^{2} \right )}{h_{n} \alpha \left (\psi(t) \right )} \right ) \right \},}
#' where \eqn{c^{\text{I}}(t) = [\Gamma(q/2)/(\pi^{q/2} n h_{n} \alpha(\psi(t)))] t^{-q/2 + 1} \psi^{\prime}(t)}, with \eqn{k} the kernel and \eqn{h_{n}} the bandwidth.
#' The function \deqn{\psi(t) = -a +  \left (a^{q/2} + t^{q/2} \right )^{2/q},}
#' with \eqn{a > 0} a tuning parameter was introduced by Liebscher (2005), and the shrinkage function
#' \eqn{\alpha(t)} yields further estimation improvement. We suggest to take (for \eqn{q > 2})
#' \deqn{\alpha(t) = 1 - \frac{1}{t^{\delta} + 1},}
#' where \eqn{\delta \in (3/4 - 1/q, 1)} is another tuning parameter. When \eqn{q = 2}, one can just take \eqn{\alpha(t) = 1}, and the value of \eqn{a} does not matter.
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
#' @seealso \code{\link{elliptselect}} for selecting optimal tuning parameters for the improved kernel estimator of the elliptical generator.
#'
#' @examples
#' q = 4
#' n = 1000 # Sample size
#' grid = seq(0.005,100,by = 0.005) # Grid on which to evaluate the elliptical generator
#' nu = 7 # Degrees of freedom
#' g_q = ((nu/(nu-2))^(q/2))*(gamma((q+nu)/2)/(((pi*nu)^(q/2))*gamma(nu/2))) * ((1+(grid/(nu-2)))^(-(q+nu)/2)) # Student-t generator with 7 degrees of freedom
#' R2 = function(t,q){(gamma((q+nu)/2)/(((nu-2)^(q/2))*gamma(nu/2)*gamma(q/2))) * (t^((q/2)-1)) * ((1+(t/(nu-2)))^(-(q+nu)/2))} # Density of squared radius
#' sample = ElliptCopulas::EllDistrSim(n,q,diag(q),density_R2 = function(t){R2(t,q)}) # Sample from 4-dimensional Student-t distribution with 7 degrees of freedom
#' # and identity covariance matrix

#' shrinkage = function(t,p){1-(1/((t^p) + 1))} # Shrinkage function
#' opt_parameters = elliptselect(n,q,seq((3/4)-(1/q)+0.01,1-0.01,len = 200),seq(0.01,2,len = 200)) # Tuning parameter selection
#' a = opt_parameters$Opta ; p = opt_parameters$Optp ; h = opt_parameters$Opth  # Optimal tuning parameters
#' g_est = elldistrest(Z = sample, Sigma_m1 = diag(q), grid = grid, h = h, a = a, shrink = function(t){shrinkage(t,p)}) # Estimated elliptical generator
#' plot(grid,g_est,type = "l", xlim = c(0,8))
#' lines(grid,g_q,col = "green")
#'
#' @export


elldistrest = function(Z, mu = 0, Sigma_m1, grid, h, Kernel = "epanechnikov", a, shrink,
                       mpfr = FALSE, precBits = 100, dopb = FALSE, normalize = 1){

  # See R package ElliptCopulas (in particular the EllDistrEst function for more details on the code)
  # The difference is that we also use the shrinkage function here, and allow for another normalization (normalize = 1)
  # such that the scale matrix equals the correlation matrix

  kernelFun = getKernel(Kernel = Kernel)

  d = ncol(Z)
  n = nrow(Z)
  n1 = length(grid)

  if(mpfr){

    a = Rmpfr::mpfr(a, precBits = precBits)
    d = Rmpfr::mpfr(d, precBits = precBits)
    grid = Rmpfr::mpfr(grid, precBits = precBits)

  }

  s_d = pi^(d/2) / gamma(d/2)
  vector_Y = rep(NA , n)
  grid_g = rep(NA, n1)

  if(dopb){pb = pbapply::startpb(max = n + n1)}

  for (i in 1:n){

    vector_Y[i] = -a + (a^(d/2) + ((Z[i,] - mu) %*% Sigma_m1 %*% (Z[i,] - mu))^(d/2))^(2/d)

    if(dopb){pbapply::setpb(pb, i)}

  }

  for (i1 in 1:n1){ # Kernel smoothing with shrinkage

    z = grid[i1]
    psiZ = as.numeric(-a + (a^(d/2) + z^(d/2))^(2/d))
    psiPZ = z^(d/2 - 1) * (a ^ (d/2) + z^(d/2)) ^ (2/d - 1)
    h_ny = (1/(h * shrink(psiZ))) * mean(kernelFun((psiZ - vector_Y)/(h * shrink(psiZ))) + kernelFun((psiZ + vector_Y)/(h * shrink(psiZ))))
    gn_z = 1/s_d * z^(-d/2 + 1) * psiPZ * h_ny
    grid_g[i1] = as.numeric(gn_z)

    if(dopb){pbapply::setpb(pb, n + i1)}

  }

  if(dopb){pbapply::closepb(pb)}

  if(normalize == 1){ # Normalize generator such that scale matrix is the covariance matrix

    grid_g = Densitygenerator.normalize(
      grid_g = grid_g, grid = grid, d = d, verbose = 0)
  }

  if(normalize == 2){ # Normalize according to ElliptCopulas package

    grid_g = ElliptCopulas::DensityGenerator.normalize(
      grid_g = grid_g, grid = grid, d = d, verbose = 0)

  }

  return(grid_g)

}

# Auxiliary functions

Densitygenerator.normalize = function(grid, grid_g, d, verbose = 0, b = 1){

  # Normalize the density generator of an elliptical distribution such that the
  # identifiability constraint of scale matrix being equal to covariance matrix is satisfied
  # Works similarly as DensityGenerator.normalize in package ElliptCopulas

  stepSize = unique(diff(grid))

  if(isTRUE(all.equal(target = 0, current = diff(stepSize) ) ) ){
    warning("The grid should be equally spaced.")
  }

  stepSize = stepSize[1]
  b = d
  s_d = 2*pi^(d/2) / gamma(d/2)
  f_integral_I1 = grid^(d/2-1) * grid_g
  integral_I1 = sum(f_integral_I1[which(is.finite(f_integral_I1))])*stepSize
  f_integral_I2 = grid^(d/2) * grid_g
  integral_I2 = sum(f_integral_I2[which(is.finite(f_integral_I2))])*stepSize
  beta_dilatation = integral_I2/(b * integral_I1)
  alpha_dilatation = 2 * beta_dilatation^(d/2) / (s_d * integral_I1)

  if(verbose > 0){
    cat("alpha = ");cat(alpha_dilatation);cat(" ; beta = ");cat(beta_dilatation)
    cat("\n")
  }

  g_normalised = dilatation(grid = grid, grid_g = grid_g,
                            alpha_dilatation = alpha_dilatation,
                            beta_dilatation = beta_dilatation)

  return(g_normalised)

}

dilatation = function(grid, grid_g, alpha_dilatation, beta_dilatation){

  # Dilatation function from package ElliptCopulas

  x = c(grid / beta_dilatation , max(grid))
  y = c(alpha_dilatation * grid_g , 0)
  f = stats::approxfun(x,y)
  g_dilate = f(grid)

  return(g_dilate)

}

getKernel = function(Kernel){

  # GetKernel function from package ElliptCopulas

  if(class(Kernel) != "character"){
    return (Kernel)
  }

  switch(Kernel,
         "gaussian" = {
           kernelFun = stats::dnorm
         },
         "epanechnikov" = {
           kernelFun = function(x){return( as.numeric(abs(x) < 1) * (1-x^2) * 3 / 4 )}
         },
         "triangular" = {
           kernelFun = function(x){return( as.numeric(abs(x) < 1) * ( 1-abs(x) ) )}
         },
         {stop("kernel ", Kernel, " not implemented yet. ",
               "Possible choices are 'gaussian', 'epanechnikov' and 'triangular'. ")}
  )

  return (kernelFun)

}

