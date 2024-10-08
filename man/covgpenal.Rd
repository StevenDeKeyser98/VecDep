% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covgpenal.R
\name{covgpenal}
\alias{covgpenal}
\title{covgpenal}
\usage{
covgpenal(
  S,
  n,
  omegas,
  derpenal = function(t, omega) {
     derSCAD(t, omega, 3.7)
 },
  nsteps = 1
)
}
\arguments{
\item{S}{The sample matrix of normal scores covariances.}

\item{n}{The sample size.}

\item{omegas}{The candidate values for the tuning parameter in \eqn{[0,\infty)}.}

\item{derpenal}{The derivative of the penalty function to be used (default = scad with parameter \eqn{a = 3.7}).}

\item{nsteps}{The number of weighted covariance graphical lasso iterations (default = 1).}
}
\value{
A list with elements "est" containing the (lasso-type) penalized matrix of sample normal scores rank correlations (output as provided by the function covglasso.R), and "omega" containing the optimal tuning parameter.
}
\description{
This function computes the empirical penalized Gaussian copula covariance matrix with the Gaussian log-likelihood
plus a lasso-type penalty as objective function.
Model selection is done by choosing omega such that BIC is maximal.
}
\details{
The aim is to solve/compute \deqn{\widehat{\boldsymbol{\Sigma}}_{\text{LT},n} \in \text{arg min}_{\boldsymbol{\Sigma} > 0} \left \{
\ln \left | \boldsymbol{\Sigma} \right | + \text{tr} \left (\boldsymbol{\Sigma}^{-1} \widehat{\boldsymbol{\Sigma}}_{n} \right ) + P_{\text{LT}}\left (\boldsymbol{\Sigma},\omega_{n} \right ) \right \},}
where the penalty function \eqn{P_{\text{LT}}} is of lasso-type:
\deqn{P_{\text{LT}} \left (\boldsymbol{\Sigma},\omega_{n} \right ) = \sum_{ij} p_{\omega_{n}} \left (\Delta_{ij} \left |\sigma_{ij} \right | \right ),}
for a certain penalty function \eqn{p_{\omega_{n}}} with penalty parameter \eqn{\omega_{n}}, and \eqn{\sigma_{ij}} the
\eqn{(i,j)}'th entry of \eqn{\boldsymbol{\Sigma}} with \eqn{\Delta_{ij} = 1} if \eqn{i \neq j} and \eqn{\Delta_{ij} = 0} if \eqn{i = j} (in order to not shrink the variances).
The matrix \eqn{\widehat{\boldsymbol{\Sigma}}_{n}} is the matrix of sample normal scores covariances.

In case \eqn{p_{\omega_{n}}(t) = \omega_{n} t} is the lasso penalty, the implementation for the
(weighted) covariance graphical lasso is available in the R package 'covglasso' (see the manual for further explanations). For general penalty functions,
we perform a local linear approximation to the penalty function and iteratively do (nsteps, default = 1) weighted covariance graphical lasso optimizations.

The default for the penalty function is the scad (derpenal = derivative of scad penalty), i.e.,
\deqn{p_{\omega_{n},\text{scad}}^{\prime}(t) = \omega_{n} \left [1 \left (t \leq \omega_{n} \right ) + \frac{\max \left (a \omega_{n} - t,0 \right )}{\omega_{n} (a-1)} 1 \left (t > \omega_{n} \right ) \right ],}
with \eqn{a = 3.7} by default.

For tuning \eqn{\omega_{n}}, we maximize (over a grid of candidate values) the BIC criterion
\deqn{\text{BIC} \left (\widehat{\boldsymbol{\Sigma}}_{\omega_{n}} \right ) = -n \left [\ln \left |\widehat{\boldsymbol{\Sigma}}_{\omega_{n}} \right | + \text{tr} \left (\widehat{\boldsymbol{\Sigma}}_{\omega_{n}}^{-1} \widehat{\boldsymbol{\Sigma}}_{n} \right ) \right ] - \ln(n) \text{df} \left (\widehat{\boldsymbol{\Sigma}}_{\omega_{n}} \right ),}
where \eqn{\widehat{\boldsymbol{\Sigma}}_{\omega_{n}}} is the estimated candidate covariance matrix using \eqn{\omega_{n}}
and df (degrees of freedom) equals the number of non-zero entries in \eqn{\widehat{\boldsymbol{\Sigma}}_{\omega_{n}}}, not taking the elements under the diagonal into account.
}
\examples{
q = 10
dim = c(5,5)
n = 100
R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
R0 = createR0(R,dim) # Sparsity on off-diagonal blocks
sample = mvtnorm::rmvnorm(n,rep(0,q),R0,method = "chol") # Sample from multivariate normal distribution
scores = matrix(0,n,q) # Normal scores
for(j in 1:q){scores[,j] = qnorm((n/(n+1)) * ecdf(sample[,j])(sample[,j]))}
Sigma_est = cov(scores) * ((n-1)/n) # Sample matrix of normal scores covariances
omega = seq(0.01, 0.6, length = 50) # Candidate tuning parameters
Sigma_est_penal = covgpenal(Sigma_est,n,omega)

}
\references{
De Keyser, S. & Gijbels, I. (2024).
High-dimensional copula-based Wasserstein dependence.
doi: https://doi.org/10.48550/arXiv.2404.07141.

Fop, M. (2021).
covglasso: sparse covariance matrix estimation, R package version 1.0.3.
url: https://CRAN.R-project.org/package=covglasso.

Wang, H. (2014).
Coordinate descent algorithm for covariance graphical lasso.
Statistics and Computing 24:521-529.
doi: https://doi.org/10.1007/s11222-013-9385-5.
}
\seealso{
\code{\link{grouplasso}} for group lasso estimation of the normal scores rank correlation matrix.
}
