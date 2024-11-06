#' @title hamse
#'
#' @description This function performs local bandwidth selection based on the amse (asymptotic mean squared error)
#'              for the beta kernel or Gaussian transformation kernel copula density estimator.
#'
#' @param input The copula argument at which the optimal local bandwidth is to be computed.
#' @param cop A fitted reference hac object, in case bw_method = 0 (default = NULL).
#' @param pseudos The (estimated) copula observations from a \eqn{q}-dimensional random vector \eqn{\mathbf{X}} (\eqn{n \times q} matrix with observations in rows, variables in columns), in case bw_method = 1 (default = NULL).
#' @param n The sample size.
#' @param estimator Either ``beta'' or ``trans'' for the beta kernel or the Gaussian transformation kernel copula density estimator.
#' @param bw_method A number in \eqn{\{0,1\}} specifying the method used for computing the bandwidth.
#'
#' @details
#' When estimator = "beta", this function computes, at a certain input, a numerical approximation of the optimal local bandwidth (for the beta kernel copula density estimator) in terms of the amse
#' (asymptotic mean squared error) given in equation (27) of De Keyser & Gijbels (2024).
#' When estimator = "trans" (for the Gaussian transformation kernel copula density estimator), this optimal bandwidth is given
#' at the end of Section 5.2 in De Keyser & Gijbels (2024).
#'
#' Of course, these optimal bandwidths depend upon the true unknown copula.
#' If bw_method = 0, then the given fitted (e.g., via MLE using \code{\link{mlehac}}) hac object (hierarchical Archimedean copula) cop is used as reference copula.
#' If bw_method = 0, then a non-parametric (beta or Gaussian transformation) kernel copula density estimator based on the pseudos as pivot is used. This pivot is computed
#' using the big O bandwidth (i.e., \eqn{n^{-2/(q+4)}} in case of the beta estimator, and \eqn{n^{-1/(q+4)}} for the transformation estimator, with \eqn{q} the total dimension).
#'
#' @return The optimal local bandwidth (in terms of amse).
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' Hierarchical variable clustering via copula-based divergence measures between random vectors.
#' International Journal of Approximate Reasoning 165:109090.
#' doi: https://doi.org/10.1016/j.ijar.2023.109090.
#'
#' @seealso \code{\link{betakernelestimator}} for the computation of the beta kernel copula density estimator,
#'
#'          \code{\link{transformationestimator}} for the computation of the Gaussian transformation kernel copula density estimator,
#'          \code{\link{phinp}} for fully non-parametric estimation of the \eqn{\Phi}-dependence between \eqn{k} random vectors.
#'
#' @examples
#' q = 4
#' dim = c(2,2)
#'
#' # Sample size
#' n = 1000
#'
#' # Four dimensional hierarchical Gumbel copula
#' # with parameters (theta_0,theta_1,theta_2) = (2,3,4)
#' HAC = gethac(dim,c(2,3,4),type = 1)
#'
#' # Sample
#' sample = suppressWarnings(HAC::rHAC(n,HAC))
#'
#' # Copula pseudo-observations
#' pseudos = matrix(0,n,q)
#' for(j in 1:q){pseudos[,j] = (n/(n+1)) * ecdf(sample[,j])(sample[,j])}
#'
#' # Maximum pseudo-likelihood estimator to be used
#' # as reference copula for bw_method = 0
#' est_cop = mlehac(sample,dim,1,c(2,3,4))
#'
#' h_1 = hamse(rep(0.5,q),cop = est_cop,n = n,estimator = "beta",bw_method = 0)
#' h_2 = hamse(rep(0.5,q),cop = est_cop,n = n,estimator = "trans",bw_method = 0)
#' h_3 = hamse(rep(0.5,q),pseudos = pseudos,n = n,estimator = "beta",bw_method = 1)
#' h_4 = hamse(rep(0.5,q),pseudos = pseudos,n = n,estimator = "trans",bw_method = 1)
#'
#' est_dens_1 = betakernelestimator(rep(0.5,q),h_1,pseudos)
#' est_dens_2 = transformationestimator(rep(0.5,q),h_2,pseudos)
#' est_dens_3 = betakernelestimator(rep(0.5,q),h_3,pseudos)
#' est_dens_4 = transformationestimator(rep(0.5,q),h_4,pseudos)
#'
#' true = HAC::dHAC(c("X1" = 0.5, "X2" = 0.5, "X3" = 0.5, "X4" = 0.5), HAC)
#'
#' @export

hamse = function(input, cop = NULL, pseudos = NULL, n, estimator, bw_method){

  q = length(input)

  if(bw_method == 0){

    if(length(childhac(cop)) == q){

      # In this case, we have a child copula, i.e. all marginals are univariate

      names = unlist(childhac(cop))

    } else{

      names = c()

      for(j in 1:q){ # Rename the variables

        names = c(names,paste("X",toString(j),sep = ""))

      }

    }

    names(input) = names
    dens = HAC::dHAC(input,cop) # Estimated density

  }

  if(bw_method == 1){

    if(estimator == "beta"){

      dens = betakernelestimator(input,n^(-2/(q+4)),pseudos) # Pivot density estimate with big oh optimal bandwidth

    }

    if(estimator == "trans"){

      dens = transformationestimator(input,n^(-1/(q+4)),pseudos) # Pivot density estimate with big oh optimal bandwidth

    }
  }

  der = matrix(0,q,2) # Element (j,i) will be approximation for i'th derivative of j'th component
  delta = 0.01 # Delta for finite difference

  for(j in 1:q){

    if(input[j] + delta  > 1){ # In case we fall out the interval [0,1] because u + delta > 1

      delta  = (1 - input[j])/2 # Make delta smaller

    }

    if(input[j] - delta < 0){ # In case we fall out the interval [0,1] because u - delta < 0

      delta = input[j]/2 # Make delta smaller

    }

    input_up = replace(input,j,input[j] + delta) # Input with u_j + delta
    input_down = replace(input,j,input[j] - delta) # Input with u_j - delta

    if(bw_method == 0){

      dens_up = HAC::dHAC(input_up,cop)
      dens_down = HAC::dHAC(input_down,cop)

    }

    if(bw_method == 1){

      if(estimator == "beta"){

        dens_up = betakernelestimator(input_up,n^(-2/(q+4)),pseudos)
        dens_down = betakernelestimator(input_down,n^(-2/(q+4)),pseudos)

      }

      if(estimator == "trans"){

        dens_up = transformationestimator(input_up,n^(-1/(q+4)),pseudos)
        dens_down = transformationestimator(input_down,n^(-1/(q+4)),pseudos)

      }
    }

    first = (dens_up-dens_down)/(2 * delta) # Approximation for first derivative w.r.t. u_j
    second = (dens_up - 2 * dens + dens_down)/(delta^2) # Approximation for second derivative w.r.t. u_j
    der[j,] = c(first,second)

  }

  if(estimator == "beta"){

    b_B = sum((1-2 * input) * der[,1]) + 0.5 * sum(input * (1-input) * der[,2])
    var_B = (((2^q) * (pi^(q/2)) * prod(sqrt(input * (1-input))))^(-1)) * dens
    term1 = ((4 * (b_B^2)) /(q * var_B))^(-2/(q+4))
    term2 = n^(-2/(q+4))

    return(term1*term2)

  }

  if(estimator == "trans"){

    b_T = 0.5 * (sum((dnorm(qnorm(input))^2) * der[,2]) - 3 * sum(qnorm(input)*dnorm(qnorm(input)) * der[,1]) + dens * (sum(qnorm(input)^2) - q))
    var_T = dens/(((4 * pi)^(q/2)) * prod(dnorm(qnorm(input))))
    term1 = ((4 * (b_T^2)) /(q * var_T))^(-1/(q+4))
    term2 = n^(-1/(q+4))

    return(term1*term2)

  }
}

