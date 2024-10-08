% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estR.R
\name{estR}
\alias{estR}
\title{estR}
\usage{
estR(
  sample,
  omega = 1,
  Q = function(t) {
     qnorm(t)
 }
)
}
\arguments{
\item{sample}{A sample from a \eqn{q}-dimensional random vector \eqn{\mathbf{X}} (\eqn{n \times q} matrix with observations in rows, variables in columns).}

\item{omega}{The penalty parameter for ridge penalization (default = 1, meaning no penalization).}

\item{Q}{The quantile function to be applied to the copula pseudo-observations (default = qnorm()).}
}
\value{
The (ridge penalized) sample \eqn{Q}-scores rank correlation matrix.
}
\description{
This function computes the sample \eqn{Q}-scores rank correlation matrix.
A ridge penalization is possible.
}
\details{
Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i} = (X_{i1}, \dots, X_{id_{i}})} a \eqn{d_{i}} dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}}, the sample \eqn{Q}-scores rank correlation matrix is given as \deqn{\widehat{\mathbf{R}}_{n} = \begin{pmatrix}
\widehat{\mathbf{R}}_{11} & \widehat{\mathbf{R}}_{12} & \cdots & \widehat{\mathbf{R}}_{1k} \\ \widehat{\mathbf{R}}_{12}^{\text{T}} & \widehat{\mathbf{R}}_{22} & \cdots & \widehat{\mathbf{R}}_{2k} \\ \vdots & \vdots & \ddots & \vdots \\ \widehat{\mathbf{R}}_{1k}^{\text{T}} & \widehat{\mathbf{R}}_{2k}^{\text{T}} & \cdots & \widehat{\mathbf{R}}_{kk}
\end{pmatrix} \hspace{0.2cm} \text{with} \hspace{0.2cm} \left (\widehat{\mathbf{R}}_{im} \right )_{jt} = \widehat{\rho}_{ij,mt} = \frac{\frac{1}{n} \sum_{\ell = 1}^{n} \widehat{Z}_{ij}^{(\ell)} \widehat{Z}_{mt}^{(\ell)}}{\frac{1}{n} \sum_{\ell = 1}^{n} \left [Q \left (\frac{\ell}{n+1} \right ) \right ]^{2}},}
for \eqn{i,m = 1, \dots, k}, \eqn{j = 1, \dots, d_{i}}, and \eqn{t = 1, \dots, d_{m}}, based on the observed \eqn{Q}-scores
\deqn{\widehat{Z}_{ij}^{(\ell)} = Q \left (\frac{n}{n+1} \widehat{F}_{ij} \left (X_{ij}^{(\ell)} \right )\right ) = Q \left (\frac{1}{n+1} \sum_{t = 1}^{n} 1 \left \{X_{ij}^{(t)} \leq X_{ij}^{(\ell)} \right \} \right ),}
for \eqn{\ell = 1, \dots, n}, where \eqn{\widehat{F}_{ij}} is the empirical cdf of the sample \eqn{X_{ij}^{(1)},\dots,X_{ij}^{(n)}} for \eqn{i = 1, \dots, k} and \eqn{j = 1, \dots, d_{i}}.
The underlying assumption is that the copula of \eqn{\mathbf{X}} is meta-elliptical.
The default for \eqn{Q} is the standard normal quantile function (corresponding to the assumption of a Gaussian copula).
Ridge penalization (especially in the Gaussian copula setting) with penalty parameter omega = \eqn{\omega} boils down to computing
\deqn{\omega \widehat{\mathbf{R}}_{n} + (1-\omega) \mathbf{I}_{q},} where \eqn{\mathbf{I}_{q}} stands for the identity matrix.
}
\examples{
# Multivariate normal copula setting
q = 10
n = 50
R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
sample = mvtnorm::rmvnorm(n,rep(0,q),R,method = "chol") # Sample from multivariate normal distribution
omega = cvomega(sample = sample,omegas = seq(0.01,0.999,len = 50),K = 5) # 5-fold cross-validation with Gaussian likelihood as loss for selecting omega
R_est = estR(sample,omega = omega)

# Multivariate Student-t copula setting
q = 10
n = 500
nu = 7 # Degrees of freedom
R2 = function(t,q){(gamma((q+nu)/2)/(((nu-2)^(q/2)) * gamma(nu/2) * gamma(q/2))) *
                   (t^((q/2)-1)) * ((1+(t/(nu-2)))^(-(q+nu)/2))} # Density of R^2, with R the radius of the elliptical distribution
# Identifiability contraint is that R is the correlation matrix
Q = function(t){extraDistr::qlst(t,nu,0,sqrt((nu-2)/nu))} # Univariate quantile function, with unit variance
R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
sample = ElliptCopulas::EllDistrSim(n,q,t(chol(R)),density_R2 = function(t){R2(t,q)}) # Sample from multivariate Student-t distribution
# with correlation matrix R and nu degrees of freedom
R_est = estR(sample,Q = Q)
}
\references{
De Keyser, S. & Gijbels, I. (2024).
Some new tests for independence among continuous random vectors.

Warton, D.I. (2008).
Penalized normal likelihood and ridge regularization of correlation and covariance matrices.
Journal of the American Statistical Association 103(481):340-349.
doi: https://doi.org/10.1198/016214508000000021.
}
\seealso{
\code{\link{cvomega}} for selecting omega using K-fold cross-validation in case of a Gaussian copula.
}
