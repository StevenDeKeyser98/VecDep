% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Icluster.R
\name{Icluster}
\alias{Icluster}
\title{Icluster}
\usage{
Icluster(data, est_method, max_dim = Inf, norm = NULL, link = "average")
}
\arguments{
\item{data}{The dataset (\eqn{n \times q} matrix with observations in rows, variables in columns) whose columns need to be clustered.}

\item{est_method}{The method for estimating the similarity between two clusters of variables.}

\item{max_dim}{The maximum dimension of the random vectors for which no link function is used when computing the similarity (default = Inf).}

\item{norm}{A possible normalization function applied to the dependence measure (default = NULL, meaning no normalization).}

\item{link}{The link function to be used when max_dim is exceeded (default = "average").}
}
\value{
A list with elements "hierarchy" containing the hierarchy constructed throughout the algorithm (a hash object), "all" containing all similarities that were computed throughout the algorithm (a hash object),
"diam" containing the average diameters of all partitions created throughout the algorithm (a vector), and "split" containing the maximum splits of all partitions created throughout the algorithm (a vector).
}
\description{
This function clusters the columns (variables) of a dataset via agglomerative hierarchical variable clustering using estimated multivariate similarities
(dependence coefficients) between random vectors.
}
\details{
Suppose that the \eqn{q} variables (of which we have \eqn{n} observations in data) are \eqn{\mathcal{S} = \{X_{1}, \dots, X_{q}\}}.
Then, most important in hierarchical variable clustering, is computing the similarity
\deqn{\mathcal{D}(\mathbb{X},\mathbb{Y})} between two disjoint subsets of variables \eqn{\mathbb{X}, \mathbb{Y} \subset \mathcal{S}}.
In particular, the main algorithm is as follows:
\itemize{
\item Each object \eqn{\{X_{i}\}} forms a separate cluster, i.e., \eqn{\aleph_{1} = \{\{X_{1}\},\dots,\{X_{q}\}\}} is the initial feature partition.}
For \eqn{i = 1,2,\dots,q-1}:
\itemize{
\item For each pair of disjoint clusters \eqn{\mathbb{X},\mathbb{Y} \in \aleph_{i}}, compute the similarity \eqn{\mathcal{D}(\mathbb{X},\mathbb{Y})}.
\item Define \eqn{\aleph_{i+1} = (\aleph_{i} \setminus \{\widetilde{\mathbb{X}},\widetilde{\mathbb{Y}}\}) \cup \{\widetilde{\mathbb{X}} \cup \widetilde{\mathbb{Y}} \}}, where \eqn{\widetilde{\mathbb{X}},\widetilde{\mathbb{Y}}} are the clusters having maximal similarity according to the previous step.
\item The algorithm stops with \eqn{\aleph_{q} = \{\{X_{1},\dots,X_{q}\}\}}.}
We call \eqn{\{\aleph_{1}, \dots, \aleph_{q}\}} the hierarchy constructed throughout the algorithm, and define, for \eqn{i \in \{1, \dots, q\}},
\eqn{\text{Adiam}(\aleph_{i}) = |\aleph_{i}|^{-1} \sum_{\mathbb{X} \in \aleph_{i}} \text{diam}(\mathbb{X})}, with
\deqn{\text{diam}(\mathbb{X}) = \begin{cases} \underset{\{X,Y\} \subset \mathbb{X} }{\min} \mathcal{D}(X,Y)  & \mbox{if } |\mathbb{X}| > 1 \\ 1 & \mbox{if } |\mathbb{X}| = 1, \end{cases}}
and \eqn{\text{Msplit}(\aleph_{i}) = \max_{\mathbb{X} \in \aleph_{i}} \text{split}(\mathbb{X})}, with
\deqn{\text{split}(\mathbb{X}) =  \underset{Y \in \aleph_{i} \setminus \mathbb{X}}{\underset{X \in \mathbb{X}}{\max}} \mathcal{D}(X,Y) \hspace{0.2cm} \text{for} \hspace{0.2cm} \{\mathbb{X}\} \neq \aleph_{i}.}
Adiam stands for the average diameter of a partition (measuring the homogeneity, which we want to be large), while
Msplit stands for the maximum split of a partition (measuring the non-separation, which we want to be small).

For measuring the similarity \eqn{\mathcal{D}(\mathbb{X},\mathbb{Y})}, we approach \eqn{\mathbb{X}} and \eqn{\mathbb{Y}} as being two random vectors
(let's say of dimensions \eqn{d_{1}} and \eqn{d_{2}} respectively). For \eqn{\mathcal{D}}, we take an estimated dependence measure between (two) random vectors. The following options are possible:
\itemize{
\item  list("phi", "mi", "Gauss", omegas = omegas) for the estimated Gaussian copula mutual information. Use omegas = 1 for no penalty, or a sequence of omegas for a ridge penalty tuned via 5-fold cross-validation,
see also the functions \code{\link{minormal}}, \code{\link{estR}}, and \code{\link{cvomega}},
\item list("phi", "Hel", "Gauss", omegas = omegas) for the estimated Gaussian copula Hellinger distance. Use omegas = 1 for no penalty, or a sequence of omegas for a ridge penalty tuned via 5-fold cross-validation,
see also the functions \code{\link{Helnormal}}, \code{\link{estR}}, and \code{\link{cvomega}},
\item list("phi", phi(t), "hac", type = type, M = M) for general \eqn{\Phi}-dependence with specified function phi(t) = \eqn{\Phi(t)},
estimated by fitting (via pseudo maximum likelihood estimation) a hierarchical Archimedean copula of given type = type,
and computed based on a Monte Carlo sample of size \eqn{M} in order to approximate the expectation, see also the functions \code{\link{mlehac}}, \code{\link{phihac}} and \code{\link{estphi}},
\item list("phi", phi(t), "nphac", estimator = estimator, type = type) for general \eqn{\Phi}-dependence with specified function phi(t) = \eqn{\Phi(t)},
estimated via non-parametric beta kernel estimation or Gaussian transformation kernel estimation,
and local bandwidth selection, by using a fitted (via pseudo maximum likelihood) hierarchical Archimedean copula
as reference copula, see also the functions \code{\link{phinp}} and \code{\link{estphi}},
\item list("phi", phi(t), "np", estimator = estimator, bw_method = bw_method) for general \eqn{\Phi}-dependence with specified function phi(t) = \eqn{\Phi(t)},
estimated via non-parametric beta kernel estimation or Gaussian transformation kernel estimation,
and local bandwidth selection, either by using a non-parametric kernel estimator as reference copula if bw_method = 1,
or by using a big O bandwidth rule if bw_method = 2, see also the functions \code{\link{phinp}} and \code{\link{estphi}},
\item list("phi", phi(t), "ellip", grid = grid) for general \eqn{\Phi}-dependence with specified function phi(t) = \eqn{\Phi(t)},
estimated via the improved MECIP procedure on the specified grid, and parameter selection done via
the function \code{\link{elliptselect}} using the Gaussian generator as reference generator, see also the functions \code{\link{phiellip}} and \code{\link{estphi}},
\item list("ot", coef = coef, omegas = omegas) for Gaussian copula Bures-Wasserstein dependence measures, either coefficient \eqn{\mathcal{D}_{1}} (coef = 1) or coefficient \eqn{\mathcal{D}_{2}} (coef = 2).
Use omegas = 1 for no penalty, or a sequence of omegas for a ridge penalty tuned via 5-fold cross-validation,
see also the functions \code{\link{bwd1}}, \code{\link{bwd2}}, \code{\link{estR}}, and \code{\link{cvomega}}.}

When \eqn{d_{1} + d_{2} >} max_dim, the specified link function (say \eqn{L}) is used
for computing the similarity between \eqn{\mathbb{X}} and \eqn{\mathbb{Y}}, i.e.,
\deqn{\mathcal{D} \left ( \mathbb{X}, \mathbb{Y} \right ) = L \left (\left \{\mathcal{D}(X,Y) : X \in \mathbb{X}, Y \in \mathbb{Y} \right \} \right ),}
which by default is the average of all inter-pairwise similarities. Other options are "single" for the minimum, and "complete" for the maximum.

The function norm (say \eqn{N}) is a possible normalization applied to the similarity measure, i.e., instead of
computing \eqn{\mathcal{D}} (using the method specified by est_method), the similarity becomes \eqn{N \circ \mathcal{D}}.
The default is \eqn{N(t) = t}, meaning that no normalization is applied.
}
\examples{
q = 20
dim = c(2,3,3,5,7) # We will impose a clustering {{X1,X2},{X3,X4,X5},{X6,X7,X8},{X9,X10,X11,X12,X13},{X14,X15,X16,X17,X18,X19,X20}}

n = 200 # Sample size

hac = gethac(dim,c(2,3,4,5,6,7),type = 1) # Twenty dimensional hierarchical Gumbel copula with parameters
# (theta_0,theta_1,theta_2,theta_3,theta_4,theta_5) = (2,3,4,5,6,7)
# So, strongest cluster is {X14,X15,X16,X17,X18,X19,X20}, then {X9,X10,X11,X12,X13},
# then {X6,X7,X8}, then {X3,X4,X5}, and finally {X1,X2}
sample = HAC::rHAC(n,hac) # Sample

# Cluster using different methods

# Gaussian copula based methods

Clustering1 =  Icluster(data = sample, est_method = list("phi", "mi", "Gauss", omegas = 1))
Clustering1$hierarchy$Aleph_16 # 5-cluster partition

Clustering2 =  Icluster(data = sample, est_method = list("phi", "mi", "Gauss", omegas = seq(0.01,0.999,len = 50)))
Clustering2$hierarchy$Aleph_16 # 5-cluster partition

Clustering3 =  Icluster(data = sample, est_method = list("phi", "mi", "Gauss", omegas = 1), max_dim = 2)
Clustering3$hierarchy$Aleph_16 # 5-cluster partition

Clustering4 =  Icluster(data = sample, est_method = list("phi", "Hel", "Gauss", omegas = 1))
Clustering4$hierarchy$Aleph_16 # 5-cluster partition

Clustering5 =  Icluster(data = sample, est_method = list("ot", coef = 1, omegas = 1))
Clustering5$hierarchy$Aleph_16 # 5-cluster partition

Clustering6 =  Icluster(data = sample, est_method = list("ot", coef = 2, omegas = 1))
Clustering6$hierarchy$Aleph_16 # 5-cluster partition

Clustering7 =  Icluster(data = sample, est_method = list("ot", coef = 2, omegas = 1), max_dim = 4)
Clustering7$hierarchy$Aleph_16 # 5-cluster partition

# Parametric hierarchical Archimedean copula approach

Clustering8 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "hac", type = 1, M = 1000), max_dim = 4)
Clustering8$hierarchy$Aleph_16 # 5-cluster partition

Clustering9 = Icluster(data = sample, est_method = list("phi", function(t){(sqrt(t)-1)^2}, "hac", type = 1, M = 1000), max_dim = 2)
Clustering9$hierarchy$Aleph_16 # 5-cluster partition

# Non-parametric approaches

Clustering10 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "nphac", estimator = "beta", type = 1), max_dim = 3)
Clustering10$hierarchy$Aleph_16 # 5-cluster partition

Clustering11 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "nphac", estimator = "trans", type = 1), max_dim = 3)
Clustering11$hierarchy$Aleph_16 # 5-cluster partition

Clustering12 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "np", estimator = "beta", bw_method = 1), max_dim = 3)
Clustering12$hierarchy$Aleph_16 # 5-cluster partition

Clustering13 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "np", estimator = "trans", bw_method = 2), max_dim = 3)
Clustering13$hierarchy$Aleph_16 # 5-cluster partition

Clustering14 = Icluster(data = sample, est_method = list("phi", function(t){(sqrt(t)-1)^2}, "np", estimator = "trans", bw_method = 1), max_dim = 2)
Clustering14$hierarchy$Aleph_16 # 5-cluster partition

# Semi-parametric meta-elliptical copula approach

Clustering15 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "ellip", grid = seq(0.005,100,by = 0.005)), max_dim = 2)
Clustering15$hierarchy$Aleph_16 # 5-cluster partition

}
\references{
De Keyser, S. & Gijbels, I. (2024).
Hierarchical variable clustering via copula-based divergence measures between random vectors.
International Journal of Approximate Reasoning 165:109090.
doi: https://doi.org/10.1016/j.ijar.2023.109090.

De Keyser, S. & Gijbels, I. (2024).
Parametric dependence between random vectors via copula-based divergence measures.
Journal of Multivariate Analysis 203:105336.
doi: https://doi.org/10.1016/j.jmva.2024.105336.

De Keyser, S. & Gijbels, I. (2024).
High-dimensional copula-based Wasserstein dependence.
doi: https://doi.org/10.48550/arXiv.2404.07141.
}
\seealso{
\code{\link{minormal}} for the computation of the Gaussian copula mutual information,
\code{\link{Helnormal}} for the computation of the Gaussian copula Hellinger distance,
\code{\link{estphi}} for several approach to estimating the \eqn{\Phi}-dependence between \eqn{k} random vectors,
\code{\link{bwd1}} for the computation of the first Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{1}},
\code{\link{bwd2}} for the computation of the second Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{2}}.
}
