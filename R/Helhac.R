#' @title Helhac
#'
#' @description This function computes the Hellinger distance between all the child copulas of a hac object obtained by the function \code{\link{gethac}}, i.e.,
#' given a \eqn{q}-dimensional random vector \eqn{\mathbf{X} = (\mathbf{X_{1}},...,\mathbf{X}_{k})} with \eqn{\mathbf{X}_{i}} a \eqn{d_{i}}-dimensional random vector, i.e., \eqn{q = d_{1} + ... + d_{k}},
#' where \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}} are connected via a hierarchical Archimedean copula with two nesting levels, \code{\link{Helhac}} computes the Hellinger distance
#' between \eqn{\mathbf{X}_{1},...,\mathbf{X}_{k}}.
#'
#' @param cop A hac object as provided by the function \code{\link{gethac}}.
#' @param dim The vector of dimensions \eqn{(d_{1},...,d_{k})}.
#' @param M The size of the Monte Carlo sample used for approximating the integral of the Hellinger distance.
#'
#' @details
#' When \eqn{\mathbf{X}} has copula density \eqn{c} with marginal copula densities \eqn{c_{i}} of \eqn{\mathbf{X}_{i}} for \eqn{i = 1, \dots, k},
#' the \eqn{\Phi}-dependence between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}} equals
#' \deqn{\mathcal{D}_{\Phi} \left (\mathbf{X}_{1}, \dots, \mathbf{X}_{k} \right ) = \int_{[0,1]^{q}} \prod_{i = 1}^{k} c_{i}(\mathbf{u}_{i}) \Phi \left (\frac{c(\mathbf{u})}{\prod_{i = 1}^{k}c_{i}(\mathbf{u}_{i})} \right ),}
#' for a certain continuous, convex function \eqn{\Phi : (0,\infty) \rightarrow \mathbb{R}}.
#' The Hellinger distance corresponds to \eqn{\Phi(t) = (\sqrt{t}-1)^{2}}, and \eqn{\mathcal{D}_{(\sqrt{t}-1)^{2}}} could be approximated by
#' \eqn{\widehat{\mathcal{D}}_{(\sqrt{t}-1)^{2}}} as implemented in the function \code{\link{phihac}}.
#' Yet, for this specific choice of \eqn{\Phi}, it is better to first simplify \eqn{\mathcal{D}_{(\sqrt{t}-1)^{2}}} to
#' \deqn{\mathcal{D}_{(\sqrt{t}-1)^{2}} \left (\mathbf{X}_{1}, \dots, \mathbf{X}_{k} \right ) = 2 - 2 \int_{[0,1]^{q}} \sqrt{c(\mathbf{u}) \prod_{i = 1}^{k} c_{i}(\mathbf{u}_{i})} d \mathbf{u},}
#' and then, by drawing a sample of size \eqn{M} from \eqn{c}, say \eqn{\mathbf{U}^{(1)}, \dots, \mathbf{U}^{(M)}}, with \eqn{\mathbf{U}^{(\ell)} = (\mathbf{U}_{1}^{(\ell)}, \dots, \mathbf{U}_{k}^{(\ell)})}, approximate it by
#' \deqn{\widetilde{D}_{(\sqrt{t}-1)^{2}} = 2 - \frac{2}{M} \sum_{\ell = 1}^{M} \sqrt{\frac{\prod_{i = 1}^{k} c_{i} \left (\mathbf{U}_{i}^{(\ell)} \right )}{c \left ( \mathbf{U}^{(\ell)} \right )}}.}
#' The function \code{\link{Helhac}} computes \eqn{\widetilde{\mathcal{D}}_{(\sqrt{t}-1)^{2}}} when \eqn{c} is a hierarchical Archimedean copula with two nesting levels,
#' as produced by the function \code{\link{gethac}}.
#'
#' @return The Hellinger distance between \eqn{\mathbf{X}_{1}, \dots, \mathbf{X}_{k}} (i.e., between all the child copulas of the hac object).
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' Parametric dependence between random vectors via copula-based divergence measures.
#' Journal of Multivariate Analysis 203:105336.
#' doi: https://doi.org/10.1016/j.jmva.2024.105336.
#'
#'
#' @seealso \code{\link{gethac}} for creating a hac object with two nesting levels,
#'          \code{\link{phihac}} for computing the \eqn{\Phi}-dependence between all the child copulas of a hac object with two nesting levels,
#'          \code{\link{mlehac}} for maximum pseudo-likelihood estimation of the parameters of a hac object with two nesting levels.
#'
#' @examples
#' \donttrun{
#' dim = c(2,2)
#' thetas = c(2,3,4)
#' HAC = gethac(dim,thetas,type = 1) # 4 dimensional nested Gumbel copula with (theta_0,theta_1,theta_2) = (2,3,4)
#' Hel = Helhac(HAC,dim,10000) # Hellinger distance based on Monte Carlo sample of size 10000
#'}
#'
#' @export


Helhac = function(cop, dim, M){

  k = length(cop$tree) - 1 # Number of random vectors
  childs = childhac(cop) # Get all child copulas
  copulasample = HAC::rHAC(M,cop) # Monte Carlo sample of size M from HAC object
  dep = integer(M) # Integrands of Hellinger distance

  for(l in 1:M){

    joint_dens = HAC::dHAC(copulasample[l,],cop) # Joint copula density
    child_dens = 1 # Product of marginal copula densities
    start = 1 # Index corresponding to first position of current random vector

    for(i in 1:k){

      sumdim = sum(dim[1:i]) # Index corresponding to last position of current random vector

      if(length(childs[[i]]) == 1){

        child_dens = child_dens # Contribution to product of marginal densities is 1 when child copula is one dimensional

      } else{

        child_dens = child_dens * HAC::dHAC(copulasample[l,start:sumdim],childs[[i]]) # Product of densities of child copulas

      }

      start = sumdim + 1 # Update index

    }

    dep[l] = sqrt(child_dens/joint_dens) # Integrand of Hellinger distance

  }

  dep = dep[!is.na(dep) & !is.infinite(dep)]

  return(2 - 2 * mean(dep))

}
