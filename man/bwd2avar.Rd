% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bwd2avar.R
\name{bwd2avar}
\alias{bwd2avar}
\title{bwd2avar}
\usage{
bwd2avar(R, dim)
}
\arguments{
\item{R}{The correlation matrix of \eqn{\mathbf{X}}.}

\item{dim}{The vector of dimensions \eqn{(d_{1},...,d_{k})}, in ascending order.}
}
\value{
The asymptotic variance of the plug-in estimator for the second Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{2}} between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
}
\description{
Given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X}_{1},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
this function computes the asymptotic variance of the plug-in estimator for the correlation-based Bures-Wasserstein coefficient \eqn{\mathcal{D}_{2}}
between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} given the entire correlation matrix \eqn{\mathbf{R}}.
The argument dim should be in ascending order.
}
\details{
The asymptotic variance of the plug-in estimator \eqn{\mathcal{D}_{2}(\widehat{\mathbf{R}}_{n})} is computed at \eqn{\mathbf{R}},
where \eqn{\widehat{\mathbf{R}}_{n}} is the sample matrix of normal scores rank correlations.
The underlying assumption is that the copula of \eqn{\mathbf{X}} is Gaussian.
}
\examples{
q = 10
dim = c(1,2,3,4)
R = 0.5^(abs(matrix(1:q-1,nrow = q, ncol = q, byrow = TRUE) - (1:q-1))) # AR(1) correlation matrix with correlation 0.5
bwd2avar(R,dim)
}
\references{
De Keyser, S. & Gijbels, I. (2024).
High-dimensional copula-based Wasserstein dependence.
doi: https://doi.org/10.48550/arXiv.2404.07141.
}
\seealso{
\code{\link{bwd1}} for the computation of the first Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{1}},
\code{\link{bwd2}} for the computation of the second Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{2}},
\code{\link{bwd1avar}} for the computation of the asymptotic variance of the plug-in estimator for \eqn{\mathcal{D}_{1}},
\code{\link{bwd1asR0}} for sampling from the asymptotic distribution of the plug-in estimator for \eqn{\mathcal{D}_{1}} under the hypothesis of independence between \eqn{\mathbf{X}_{1},\dots,\mathbf{X}_{k}},
\code{\link{bwd2asR0}} for sampling from the asymptotic distribution of the plug-in estimator for \eqn{\mathcal{D}_{2}} under the hypothesis of independence between \eqn{\mathbf{X}_{1},\dots,\mathbf{X}_{k}},
\code{\link{estR}} for the computation of the sample matrix of normal scores rank correlations,
\code{\link{sortot}} for rearranging the columns of sample such that dim is in ascending order.
}
