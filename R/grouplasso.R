#' @title grouplasso
#'
#' @description Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' this function computes the empirical penalized Gaussian copula covariance matrix with the Gaussian log-likelihood
#' plus the grouped lasso penalty as objective function, where the groups are the diagonal and off-diagonal blocks corresponding to the different
#' random vectors.
#' Model selection is done by choosing omega such that BIC is maximal.
#'
#' @param Sigma An initial guess for the covariance matrix (typically equal to S).
#' @param S     The sample matrix of normal scores covariances.
#' @param n     The sample size.
#' @param omegas The candidate values for the tuning parameter in \eqn{[0,\infty)}.
#' @param dim    The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#' @param step.size The step size used in the generalized gradient descent, affects the speed of the algorithm (default = 100).
#'
#' @details
#' Given a covariance matrix \deqn{\boldsymbol{\Sigma} = \begin{pmatrix} \boldsymbol{\Sigma}_{11} & \boldsymbol{\Sigma}_{12} & \cdots & \boldsymbol{\Sigma}_{1k} \\
#'                                                              \boldsymbol{\Sigma}_{12}^{\text{T}} & \boldsymbol{\Sigma}_{22} & \cdots & \boldsymbol{\Sigma}_{2k} \\
#'                                                              \vdots & \vdots & \ddots & \vdots \\
#'                                                              \boldsymbol{\Sigma}_{1k}^{\text{T}} & \boldsymbol{\Sigma}_{2k}^{\text{T}} & \cdots & \boldsymbol{\Sigma}_{kk} \end{pmatrix},}
#' the aim is to solve/compute \deqn{\widehat{\boldsymbol{\Sigma}}_{\text{GLT},n} \in \text{arg min}_{\boldsymbol{\Sigma} > 0} \left \{
#' \ln \left | \boldsymbol{\Sigma} \right | + \text{tr} \left (\boldsymbol{\Sigma}^{-1} \widehat{\boldsymbol{\Sigma}}_{n} \right ) + P_{\text{GLT}}\left (\boldsymbol{\Sigma},\omega_{n} \right ) \right \},}
#' where the penalty function \eqn{P_{\text{GLT}}} is of group lasso-type:
#' \deqn{P_{\text{GLT}} \left (\boldsymbol{\Sigma},\omega_{n} \right ) = 2 \sum_{i,j = 1, j > i}^{k} p_{\omega_{n}} \left (\sqrt{d_{i}d_{j}} \left | \left |\boldsymbol{\Sigma}_{ij} \right | \right |_{\text{F}} \right )
#' + \sum_{i = 1}^{k} p_{\omega_{n}} \left (\sqrt{d_{i}(d_{i}-1)} \left | \left | \boldsymbol{\Delta}_{i} * \boldsymbol{\Sigma}_{ii} \right | \right |_{\text{F}} \right ), }
#' for a certain penalty function \eqn{p_{\omega_{n}}} with penalty parameter \eqn{\omega_{n}}, and
#' \eqn{\boldsymbol{\Delta}_{i} \in \mathbb{R}^{d_{i} \times d_{i}}} a matrix with ones as off-diagonal elements and zeroes
#' on the diagonal (in order to avoid shrinking the variances, the operator \eqn{*} stands for elementwise multiplication).
#'
#' For now, the only possibility in this function for \eqn{p_{\omega_{n}}} is the lasso penalty \eqn{p_{\omega_{n}}(t) = \omega_{n} t}.
#' For other penalties (e.g., scad), one can do a local linear approximation to the penalty function and iteratively perform weighted group lasso optimizations (similar to what is done in the function \code{\link{covgpenal}}).
#'
#' Regarding the implementation, we used the code available in the R package \sQuote{spcov} (see the manual for further explanations),
#' but altered it to the context of a group-lasso penalty.
#'
#' For tuning \eqn{\omega_{n}}, we maximize (over a grid of candidate values) the BIC criterion
#' \deqn{\text{BIC} \left (\widehat{\boldsymbol{\Sigma}}_{\omega_{n}} \right ) = -n \left [\ln \left |\widehat{\boldsymbol{\Sigma}}_{\omega_{n}} \right | + \text{tr} \left (\widehat{\boldsymbol{\Sigma}}_{\omega_{n}}^{-1} \widehat{\boldsymbol{\Sigma}}_{n} \right ) \right ] - \ln(n) \text{df} \left (\widehat{\boldsymbol{\Sigma}}_{\omega_{n}} \right ),}
#' where \eqn{\widehat{\boldsymbol{\Sigma}}_{\omega_{n}}} is the estimated candidate covariance matrix using \eqn{\omega_{n}}
#' and df (degrees of freedom) equals
#' \deqn{ \hspace{-3cm} \text{df} \left (\widehat{\boldsymbol{\Sigma}}_{\omega_{n}} \right ) = \sum_{i,j = 1, j > i}^{k} 1 \left (\left | \left | \widehat{\boldsymbol{\Sigma}}_{\omega_{n},ij} \right | \right |_{\text{F}} > 0 \right ) \left (1 + \frac{\left | \left | \widehat{\boldsymbol{\Sigma}}_{\omega_{n},ij} \right | \right |_{\text{F}}}{\left | \left | \widehat{\boldsymbol{\Sigma}}_{n,ij} \right | \right |_{\text{F}}} \left (d_{i}d_{j} - 1 \right ) \right )}
#' \deqn{ \hspace{2cm} + \sum_{i = 1}^{k} 1 \left (\left | \left |\boldsymbol{\Delta}_{i} * \widehat{\boldsymbol{\Sigma}}_{\omega_{n},ii} \right | \right |_{\text{F}} > 0  \right )
#' \left (1 + \frac{\left | \left |\boldsymbol{\Delta}_{i} * \widehat{\boldsymbol{\Sigma}}_{\omega_{n},ii} \right | \right |_{\text{F}}}{\left | \left |\boldsymbol{\Delta}_{i} * \widehat{\boldsymbol{\Sigma}}_{n,ii} \right | \right |_{\text{F}}} \left (\frac{d_{i} \left ( d_{i} - 1 \right )}{2} - 1 \right )  \right ) + q,}
#' with \eqn{\widehat{\boldsymbol{\Sigma}}_{\omega_{n},ij}} the \eqn{(i,j)}'th block of \eqn{\widehat{\boldsymbol{\Sigma}}_{\omega_{n}}}, similarly for \eqn{\widehat{\boldsymbol{\Sigma}}_{n,ij}}.
#'
#' @return A list with elements "est" containing the (group lasso) penalized matrix of sample normal scores rank correlations (output as provided by the function \dQuote{spcov.R}), and "omega" containing the optimal tuning parameter.
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' High-dimensional copula-based Wasserstein dependence.
#' doi: https://doi.org/10.48550/arXiv.2404.07141.
#'
#' Bien, J. & Tibshirani, R. (2022).
#' spcov: sparse estimation of a covariance matrix, R package version 1.3.
#' url: https://CRAN.R-project.org/package=spcov.
#'
#' Bien, J. & Tibshirani, R. (2011).
#' Sparse Estimation of a Covariance Matrix.
#' Biometrika 98(4):807-820.
#' doi: https://doi.org/10.1093/biomet/asr054.
#'
#' @seealso \code{\link{covgpenal}} for (elementwise) lasso-type estimation of the normal scores rank correlation matrix.
#'
#' @examples
#' \dontrun{
#' q = 10
#' dim = c(5,5)
#' n = 100
#'
#' # AR(1) correlation matrix with correlation 0.5
#' R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1)))
#'
#' # Sparsity on off-diagonal blocks
#' R0 = createR0(R,dim)
#'
#' # Sample from multivariate normal distribution
#' sample = mvtnorm::rmvnorm(n,rep(0,q),R0,method = "chol")
#'
#' # Normal scores
#' scores = matrix(0,n,q)
#' for(j in 1:q){scores[,j] = qnorm((n/(n+1)) * ecdf(sample[,j])(sample[,j]))}
#'
#' # Sample matrix of normal scores covariances
#' Sigma_est = cov(scores) * ((n-1)/n)
#'
#' # Candidate tuning parameters
#' omega = seq(0.01, 0.6, length = 50)
#'
#' Sigma_est_penal = grouplasso(Sigma_est, Sigma_est, n, omega, dim)
#'}
#' @export

grouplasso = function(Sigma, S, n, omegas, dim, step.size = 100){

  q = nrow(S)
  bic = integer(length(omegas)) # BIC values
  ests = list() # Estimates

  for(o in 1:length(omegas)){

    omega = omegas[o]
    gspcov = Gspcov(Sigma, S, omega, dim, step.size) # Perform group lasso estimation
    names(gspcov)[[2]] = "sigma"
    sigma = gspcov$sigma # Estimate
    penal = df(sigma,S,dim) * log(n) # Compute penalty
    bic[o] = -n*(log(det(sigma)) + sum(diag(solve(sigma) %*% S))) - penal # Compute BIC
    ests[[o]] = gspcov

  }

  best = which.max(bic) # Position for which BIC is maximal

  return(list("est" = ests[[best]], "omega" = omegas[best])) # Best estimate with corresponding omega parameter

}

# Auxiliary functions

df = function(Sigma,S,dim){

  # Degrees of freedom for group lasso
  # Sigma is the candidate covariance matrix estimate
  # S is the sample matrix of normal scores covariances
  # dim = c(d_1,...,d_k)

  q = nrow(S)
  df = 0
  start = 1 # Index corresponding to first position of current random vector

  # Upper off-diagonal blocks

  for(i in 1:(length(dim)-1)){

    sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector

    for(j in i:(length(dim)-1)){

      sumdim2 = sum(dim[1:j]) # Index corresponding to first position of next random vector - 1
      blockSigma = Sigma[start:sumdim,(sumdim2 + 1):(sumdim2 + dim[j+1])] # Sigma block
      blockS = S[start:sumdim,(sumdim2 + 1):(sumdim2 + dim[j+1])] # S block
      normblockSigma = sqrt(sum(blockSigma^2)) # Frobenius norm of Sigma block
      normblockS = sqrt(sum(blockS^2)) # Frobenius norm of S block
      df = df + as.numeric(normblockSigma > 0) * (1 + ((normblockSigma/normblockS) * ((dim[i] * dim[j+1]) - 1)))

    }

    start = sumdim + 1

  }

  start = 1

  # Diagonal blocks

  for(i in 1:length(dim)){

    sumdim = sum(dim[1:i])

    if(dim[i] == 1){

      df = df

    } else{

      delta = matrix(1,dim[i],dim[i]) - diag(dim[i]) # Deltai matrix
      blockSigma = Sigma[start:sumdim,start:sumdim]
      blockS = S[start:sumdim,start:sumdim]
      normblockSigma = sqrt(sum((delta * blockSigma)^2))
      normblockS = sqrt(sum((delta * blockS)^2))
      df = df + as.numeric(normblockSigma > 0) * (1 + ((normblockSigma/normblockS) * (((dim[i] * (dim[i] - 1))/2) - 1)))

    }

    start = sumdim + 1

  }

  df = df + q

  return(df)

}

Gspcov = function(Sigma, S, omega, dim, step.size, nesterov = TRUE,
                  n.outer.steps = 1e4, n.inner.steps = 1e4,
                  tol.outer = 1e-4, thr.inner = 1e-2,
                  backtracking = 0.2, trace = 0){

  # Computes the group lasso estimator where Frobenius penalty is imposed
  # on all off-diagonal blocks, and all diagonal blocks, but no penalization of
  # diagonal elements
  # Sigma is an initial guess for the covariance matrix
  # S is the sample matrix of normal scores covariances
  # dim contains the dimensions of the random vectors
  # See R package spcov for explanations of other (default) arguments and code

  if(all(omega == 0)){

    cat("Skipping MM.  Solution is S!", fill = T)

    return(list(n.iter=0, Sigma=S, obj=ComputeObjective(S, S, omega, dim = dim)))

  }

  stopifnot(omega >= 0)

  if(trace > 0){

    cat("---", fill = T)
    cat(ifelse(nesterov, "using Nesterov, ", ""))
    cat(ifelse(backtracking, "backtracking line search", ""), fill = T)
    cat("---", fill = T)

  }

  mean.abs.S = mean(abs(S))

  if(min(eigen(Sigma, symmetric = T, only.values = T)$val) < 1e-5)

    warning("Starting value is nearly singular.")

  del = ComputeDelta(S, omega, trace = trace - 1, dim = dim)
  objective = ComputeObjective(Sigma, S, omega, dim = dim)

  if(trace > 0)

    cat("objective: ", objective, fill = T)

  n.iter = NULL

  for(i in seq(n.outer.steps)){

    Sigma0 = Sigma

    if(trace > 0)

      cat("step size given to GGDescent/Nesterov:", step.size, fill = T)

    gg = GGDescent(Sigma = Sigma, Sigma0 = Sigma0, S = S, omega = omega,
                   del = del, nsteps = n.inner.steps,
                   step.size = step.size,
                   nesterov = nesterov,
                   tol = thr.inner * mean.abs.S,
                   trace = trace - 1,
                   backtracking = backtracking, dim = dim)

    Sigma = gg$Sigma
    objective = c(objective, ComputeObjective(Sigma, S, omega, dim = dim))

    if(trace > 0){

      cat("objective: ", objective[length(objective)],
          " (", gg$niter, "iterations, max step size:",
          max(gg$step.sizes), ")", fill = T)

    }

    if(backtracking){

      if(max(gg$step.sizes) < step.size * backtracking ^ 2){

        step.size = step.size * backtracking

        if(trace > 0)

          cat("Reducing step size to", step.size, fill = T)

      }
    }

    n.iter = c(n.iter, gg$niter)

    if(objective[i + 1] > objective[i] - tol.outer){

      cat("MM converged in", i, "steps!", fill = T)

      break

    }
  }

  list(n.iter = n.iter, Sigma = gg$Sigma, obj = objective)

}

GGDescent = function(Sigma, Sigma0, S, omega, del, nsteps,
                     step.size, nesterov = FALSE, backtracking = FALSE,
                     tol = 1e-3, trace = 0, dim){

  # Generalized gradient descent steps
  # See spcov package for explanation of most of the code
  # For group lasso, the ComputeObjective function is different

  if(backtracking){

    beta = backtracking

    if (beta <= 0 | beta >= 1)

      stop("Backtracking parameter beta must be in (0,1).")

  }

  tt = step.size
  converged = FALSE
  exit = FALSE
  obj.starting = ComputeObjective(Sigma, S, omega, dim = dim)
  Sigma.starting = Sigma
  Omega = Sigma
  Sigma.last = Sigma
  ttts = NULL
  ttt = tt

  for(i in seq(nsteps)){

    inv.Sigma0 = solve(Sigma0)
    log.det.Sigma0 = LogDet(Sigma0)
    grad.g = ComputeGradientOfg(Omega, S, Sigma0, inv.Sigma0 = inv.Sigma0)
    grad.g = (grad.g + t(grad.g)) / 2
    g.omega = g(Omega, S, Sigma0, inv.Sigma0 = inv.Sigma0, log.det.Sigma0 = log.det.Sigma0)

    while(backtracking){

      soft.thresh = ProxADMM(Omega - ttt * grad.g, del, omega = omega * ttt, dim = dim, rho = .1)$X
      gen.grad.g = (Omega - soft.thresh) / ttt
      left = g(soft.thresh, S, Sigma0,inv.Sigma0 = inv.Sigma0, log.det.Sigma0 = log.det.Sigma0)
      right = g.omega - ttt * sum(grad.g * gen.grad.g) + ttt * sum(gen.grad.g ^ 2) / 2

      if(is.na(left) || is.na(right)){

        print("left or right is NA.")
        # browser()

      }

      if(left <= right){

        Sigma = soft.thresh
        ttts = c(ttts, ttt)

        if(mean(abs(Sigma - Sigma.last)) < tol){

          converged = TRUE
          break

        }

        if(nesterov)

          Omega = Sigma + (i - 1) / (i + 2) * (Sigma - Sigma.last)

        else

          Omega = Sigma
        Sigma.last = Sigma

        if(trace > 0)

          cat("--true objective:", ComputeObjective(Sigma, S, omega, dim = dim), fill = T)

        if(trace > 0)

          cat(i, ttt, " ")

        break

      }

      ttt = beta * ttt

      if(ttt < 1e-15){

        cat("Step size too small: no step taken", fill = T)
        exit = TRUE
        break

      }
    }

    if(!backtracking){

      Sigma = ProxADMM(Sigma - ttt * grad.g, del, omega = omega * ttt, dim = dim, rho=.1)$X

      if(mean(abs(Sigma - Sigma.last)) < tol)

        converged = TRUE

      if(nesterov)

        Omega = Sigma + (i - 1)/(i + 2) * (Sigma - Sigma.last)

      else

        Omega = Sigma
      Sigma.last = Sigma

    }

    if(converged){

      if(trace > 0){

        cat("--GG converged in", i, "steps!")

        if(backtracking)

          cat(" (last step size:", ttt, ")", fill = T)

        else
          cat(fill = T)
      }

      break
    }


    if(exit){

      break

    }
  }

  obj.end = ComputeObjective(Sigma, S, omega, dim = dim)

  if(obj.starting < obj.end){

    if(nesterov){

      cat("Objective rose with Nesterov.  Using generalized gradient instead.", fill = T)

      return(GGDescent(Sigma = Sigma.starting, Sigma0 = Sigma0, S = S, omega = omega,
                       del = del, nsteps = nsteps, step.size = step.size,
                       nesterov = FALSE,
                       backtracking = backtracking,
                       tol = tol, trace = trace, dim = dim))
    }

    # browser()
    cat("--Returning initial Sigma since GGDescent/Nesterov did not decrease objective", fill=T)
    Sigma = Sigma.starting

  }

  list(Sigma = Sigma, niter = i, step.sizes = ttts)

}


ComputeObjective = function(Sigma, S, omega, dim){

  # Group lasso objective function

  -2 * ComputeLikelihood(Sigma, S) + ComputePenalty(Sigma, omega, dim = dim)

}

ComputeLikelihood = function(Sigma, S){

  # Gaussian log-likelihood

  p = nrow(Sigma)
  ind.diag = 1 + 0:(p - 1) * (p + 1)
  -(1 / 2) * (LogDet(Sigma) + sum(solve(Sigma, S)[ind.diag]))

}

ComputePenalty = function(Sigma, omega, dim){

  # Group lasso penalty

  penal = 0
  start = 1 # Index corresponding to first position of current random vector

  # Upper off-diagonal blocks

  for(i in 1:(length(dim)-1)){

    sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector

    for(j in i:(length(dim)-1)){

      sumdim2 = sum(dim[1:j]) # Index corresponding to first position of next random vector - 1
      block = Sigma[start:sumdim,(sumdim2 + 1):(sumdim2 + dim[j+1])] # Sigma block
      penal = penal + sqrt(dim[i] * dim[j+1] * sum(block^2))

    }

    start = sumdim + 1

  }

  penal = 2 * omega * penal
  start = 1
  penal2 = 0

  # Diagonal blocks

  for(i in 1:length(dim)){

    sumdim = sum(dim[1:i])
    delta = matrix(1,dim[i],dim[i]) - diag(dim[i])
    block = Sigma[start:sumdim,start:sumdim]
    penal2 = penal2 + sqrt(dim[i] * (dim[i] - 1) * sum((delta * block)^2))
    start = sumdim + 1

  }

  return(penal + omega * penal2)

}

LogDet = function(M) {

  # Log of determinant of matrix M, see spcov package

  log.det = determinant(M, logarithm = TRUE)

  if(log.det$sign == -1)

    return(NA)

  as.numeric(log.det$mod)

}

h = function(x, a) log(x) + a / x

dhdx = function(x, a) (1 - a / x) / x

FindMinSolution = function(a, c, tol = 1e-10, max.iter = 1e3, trace = 1){

  # See spcov package

  if (h(a, a) > c)

    stop("No solution!")

  if (h(a, a) == c)

    return(a)


  x.min = 0.1 * a

  for(i in seq(max.iter)){

    if(trace > 1)

      cat("h(", x.min, ", a) = ", h(x.min, a), fill=T)
    x.min = x.min - (h(x.min, a) - c) / dhdx(x.min, a)

    if(x.min <= 0)
      x.min = a * tol

    else if(x.min >= a)

      x.min = a * (1 - tol)

    if(abs(h(x.min, a) - c) < tol & h(x.min, a) >= c){

      if(trace > 0)

        cat("Eval-bound converged in ", i, "steps to ", x.min, fill=T)

      break

    }
  }

  x.min

}

ComputeDelta = function(S, omega, Sigma.tilde = NULL, trace = 1, dim){

  # See spcov package

  if(is.null(Sigma.tilde))

    Sigma.tilde = diag(diag(S))

  p = nrow(S)
  f.tilde = ComputeObjective(Sigma.tilde, S, omega, dim = dim)
  minev = min(eigen(S)$val)
  c = f.tilde - (p - 1) * (log(minev) + 1)
  FindMinSolution(a = minev, c = c, trace = trace)

}

ComputeGradientOfg = function(Sigma, S, Sigma0, inv.Sigma0 = NULL){

  # See spcov package

  inv.Sigma = solve(Sigma)

  if(is.null(inv.Sigma0))

    solve(Sigma0) - inv.Sigma %*% S %*% inv.Sigma

  else

    inv.Sigma0 - inv.Sigma %*% S %*% inv.Sigma
}

g = function(Sigma, S, Sigma0, inv.Sigma0 = NULL, log.det.Sigma0 = NULL) {

  # See spcov package

  p = nrow(Sigma)
  ind.diag = 1 + 0:(p - 1) * (p + 1)

  if (is.null(log.det.Sigma0))

    log.det.Sigma0 = LogDet(Sigma0)

  if (is.null(inv.Sigma0))

    log.det.Sigma0 + sum((solve(Sigma0, Sigma) + solve(Sigma, S))[ind.diag]) - p

  else

    log.det.Sigma0 + sum((inv.Sigma0 %*% Sigma + solve(Sigma, S))[ind.diag]) - p

}

ProxADMM = function(A, del, omega, dim, rho = .1, tol = 1e-6, maxiters = 100, verb = FALSE){

  # See spcov package

  soft = SoftThreshold(A, omega, dim)
  minev = min(eigen(soft, symmetric = T, only.values = T)$val)

  if(minev >= del){

    return(list(X = soft, Z = soft, obj = ComputeProxObjective(soft, A, omega, dim = dim)))

  }

  p = nrow(A)
  obj = NULL

  Z = soft
  Y = matrix(0, p, p)

  for(i in seq(maxiters)){

    B = (A + rho * Z - Y) / (1 + rho)

    if(min(eigen(B, symmetric = T, only.values = T)$val) < del){

      eig = eigen(B, symmetric = T)
      X = eig$vec %*% diag(pmax(eig$val, del)) %*% t(eig$vec)

    }

    else{

      X = B

    }

    obj = c(obj, ComputeProxObjective(X, A, omega, dim = dim))

    if(verb)

      cat(" ", obj[i], fill = T)

    if(i > 1)

      if(obj[i] > obj[i - 1] - tol){

        if(verb)
          cat(" ADMM converged after ", i, " steps.", fill = T)

        break

      }

    Z = SoftThreshold(X + Y / rho, omega / rho, dim = dim)
    Y = Y + rho * (X - Z)

  }

  list(X = X, Z = Z, obj = obj)
}

SoftThreshold = function(x, omega, dim){

  # Elementwise soft thresholding operation in case of group lasso
  # x is sample matrix of normal scores covariances
  # Omega is the penalty parameter
  # dim contains the dimensions of the random vectors

  q = nrow(x)
  Sol = matrix(0,q,q) # Solution
  start = 1 # Index corresponding to first position of current random vector

  # Upper off-diagonal blocks

  for(i in 1:(length(dim)-1)){

    sumdim = sum(dim[1:i]) # Index corresponding to position of current random vector

    for(j in i:(length(dim)-1)){

      sumdim2 = sum(dim[1:j]) # Index corresponding to first position of next random vector - 1
      block = x[start:sumdim,(sumdim2 + 1):(sumdim2 + dim[j+1])]
      S = sqrt(sum(block^2)) ; gamma = sqrt(dim[i] * dim[j+1]) # Si,m and gammai,m

      if(S <= omega * gamma){

        Sol[start:sumdim,(sumdim2 + 1):(sumdim2 + dim[j+1])] = matrix(0,dim[i],dim[j+1])

      } else{

        Sol[start:sumdim,(sumdim2 + 1):(sumdim2 + dim[j+1])] = block * (1 - ((omega * gamma) / S))

      }

      Sol[(sumdim2 + 1):(sumdim2 + dim[j+1]),start:sumdim] = t(Sol[start:sumdim,(sumdim2 + 1):(sumdim2 + dim[j+1])])

    }

    start = sumdim + 1

  }

  start = 1

  # Diagonal blocks

  for(i in 1:length(dim)){

    sumdim = sum(dim[1:i])
    delta = matrix(1,dim[i],dim[i]) - diag(dim[i])
    block = x[start:sumdim,start:sumdim]
    S = sqrt(sum((delta * block)^2)) ; gamma = sqrt(dim[i] * (dim[i] - 1))

    if(S <= omega * gamma){

      Sol[start:sumdim,start:sumdim] = matrix(0,dim[i],dim[i])

    } else{

      Sol[start:sumdim,start:sumdim] = block * (1 - ((omega * gamma) / S))

    }

    if(length(start:sumdim) == 1){

      Sol[start:sumdim,start:sumdim] = block

    } else{

      diag(Sol[start:sumdim,start:sumdim]) = diag(block)

    }

    start = sumdim + 1

  }

  return(Sol)

}

ComputeProxObjective = function(X, A, omega, dim){

  # Objective of elementwise soft thresholding operation

  sum((X-A)^2) / 2 + ComputePenalty(X, omega, dim)

}
