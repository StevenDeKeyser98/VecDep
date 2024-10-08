% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phihac.R
\name{phihac}
\alias{phihac}
\title{phihac}
\usage{
phihac(cop, dim, M, phi)
}
\arguments{
\item{cop}{A hac object as provided by the function \code{\link{gethac}}.}

\item{dim}{The vector of dimensions \eqn{(d_{1},...,d_{k})}.}

\item{M}{The size of the Monte Carlo sample used for approximating the integral of the \eqn{\Phi}-dependence.}

\item{phi}{The function \eqn{\Phi}.}
}
\value{
The \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}} (i.e., between all the child copulas of the hac object).
}
\description{
This function computes the \eqn{\Phi}-dependence between all the child copulas of a hac object obtained by the function \code{\link{gethac}}, i.e.,
given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X_{1}},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
where \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} are connected via a hierarchical Archimedean copula with two nesting levels, phihac computes the \eqn{\Phi}-dependence
between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
}
\details{
When \eqn{\mathbf{X}} has copula density \eqn{c} with marginal copula densities \eqn{c_{i}} of \eqn{\mathbf{X}_{i}} for \eqn{i = 1, \dots, k},
the \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}} equals
\deqn{\mathcal{D}_{\Phi} \left (\mathbf{X}_{1}, \dots, \mathbf{X}_{k} \right ) = \int_{[0,1]^{q}} \prod_{i = 1}^{k} c_{i}(\mathbf{u}_{i}) \Phi \left (\frac{c(\mathbf{u})}{\prod_{i = 1}^{k}c_{i}(\mathbf{u}_{i})} \right ),}
for a certain continuous, convex function \eqn{\Phi : (0,\infty) \rightarrow \mathbb{R}}.
By drawing a sample of size \eqn{M} from \eqn{c}, say \eqn{\mathbf{U}^{(1)}, \dots, \mathbf{U}^{(M)}}, with \eqn{\mathbf{U}^{(\ell)} = (\mathbf{U}_{1}^{(\ell)}, \dots, \mathbf{U}_{k}^{(\ell)})}, we can approximate \eqn{\mathcal{D}_{\Phi}} by
\deqn{\widehat{\mathcal{D}}_{\Phi} = \frac{1}{M} \sum_{\ell = 1}^{M} \frac{\prod_{i = 1}^{k} c_{i} \left (\mathbf{U}_{i}^{(\ell)} \right )}{c \left (\mathbf{U}^{(\ell)} \right )} \Phi \left (\frac{c \left (\mathbf{U}^{(\ell)} \right )}{\prod_{i = 1}^{k} c_{i} \left (\mathbf{U}_{i}^{(\ell)} \right )} \right ).}
The function \code{\link{phihac}} computes \eqn{\widehat{\mathcal{D}}_{\Phi}} when \eqn{c} is a hierarchical Archimedean copula with two nesting levels,
as produced by the function \code{\link{gethac}}.
}
\examples{
dim = c(2,2)
thetas = c(2,3,4)
HAC = gethac(dim,thetas,type = 1) # 4 dimensional nested Gumbel copula with (theta_0,theta_1,theta_2) = (2,3,4)
Phi_dep = phihac(HAC,dim,10000,function(t){t * log(t)}) # Mutual information based on Monte Carlo sample of size 10000

}
\references{
De Keyser, S. & Gijbels, I. (2024).
Parametric dependence between random vectors via copula-based divergence measures.
Journal of Multivariate Analysis 203:105336.
doi: https://doi.org/10.1016/j.jmva.2024.105336.
}
\seealso{
\code{\link{gethac}} for creating a hac object with two nesting levels,
\code{\link{Helhac}} for computing the Hellinger distance between all the child copulas of a hac object with two nesting levels,
\code{\link{mlehac}} for maximum pseudo-likelihood estimation of the parameters of a hac object with two nesting levels.
}
