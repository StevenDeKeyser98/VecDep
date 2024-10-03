#' @title elliptselect
#'
#' @description This functions selects optimal tuning parameters for improved kernel estimation of the generator of an elliptical distribution.
#'
#' @param n The sample size.
#' @param q The total dimension.
#' @param pseq Candidate values for the \eqn{\delta} parameter of the shrinkage function.
#' @param aseq Candidate values for the \eqn{a} parameter of the Liebscher function.
#'
#' @details
#' When using the function \code{\link{elldistrest}} for estimating an elliptical generator \eqn{g_{\mathcal{R}}}
#' based on a kernel \eqn{k} with bandwidth \eqn{h_{n}}, the function
#' \deqn{\psi(t) = -a +  \left (a^{q/2} + t^{q/2} \right )^{2/q},} and the shrinkage function (for \eqn{q > 3})
#' \deqn{\alpha(t) = 1 - \frac{1}{t^{\delta} + 1},} this function selects \eqn{h_{n}, \delta} and \eqn{a} in the following way.
#'
#' Use the normal generator \eqn{g_{\mathcal{R}}(t) = e^{-t/2}/(2 \pi)^{q/2}} as reference generator, and define
#' \deqn{\Psi(t) = \frac{\pi^{q/2}}{\Gamma(q/2)} \left (\psi^{-1}(t) \right )^{\prime} \left (\psi^{-1}(t) \right )^{q/2 - 1} g_{\mathcal{R}} \left (\psi^{-1}(t) \right ),} as well as
#' \deqn{h_{n}^{\text{opt}} = \left \{\frac{\left (\int_{-1}^{1} k^{2}(t) dt \right ) \left (\int_{0}^{\infty} \alpha(t)^{-1} \Psi(t) dt \right )}{\left (\int_{-1}^{1} t^{2} k(t) dt \right )^{2} \left (\int_{0}^{\infty} \left (\alpha(t)^{2} \Psi^{\prime \prime}(t) \right )^{2} dt \right )} \right \}^{1/5} n^{-1/5}.}
#'
#'
#' When \eqn{q = 2}, take \eqn{\alpha(t) = 1} (there is no need for shrinkage), and take \eqn{h_{n}^{\text{opt}}}. The value of \eqn{a} does not matter.
#'
#' When \eqn{q > 2}, specify a grid of candidate \eqn{\delta}-values in \eqn{(3/4 - 1/q,1)} and a grid of \eqn{a}-values in \eqn{(0, \infty)}.
#' For each of these candidate values, compute the corresponding optimal (AMISE) bandwidth \eqn{h_{n}^{\text{opt}}}.
#' Take the combination of parameters that minimizes (a numerical approximation of) the (normal reference) AMISE given in equation (20) of De Keyser & Gijbels (2024).
#'
#' @return A list with elements "Opta" containing the optimal \eqn{a}, "Optp" containing the optimal \eqn{\delta}, and "Opth" containing the optimal \eqn{h_{n}}.
#'
#' @references
#'
#' De Keyser, S. & Gijbels, I. (2024).
#' Hierarchical variable clustering via copula-based divergence measures between random vectors.
#' International Journal of Approximate Reasoning 165:109090.
#' doi: https://doi.org/10.1016/j.ijar.2023.109090.
#'
#' @seealso \code{\link{elldistrest}} for improved kernel estimation of the elliptical generator.
#'
#' @examples
#' q = 4
#' n = 1000
#' opt_parameters = elliptselect(n,q,seq((3/4)-(1/q)+0.01,1-0.01,len = 200),seq(0.01,2,len = 200))
#'
#' @export


elliptselect = function(n, q, pseq, aseq){

  if(q == 2){ #  When q = 2, there is no need for the parameter a or shrinkage (r = Inf), we can just calculate the h_AMISE bandwidth

    Optp = 1 ; Opta = 0
    Opth = bw_ellip(Opta,q,Optp,Inf,n)

  }

  else{

    r = 1 # We take r = 1 when q > 2
    errors = matrix(0,length(aseq),length(pseq))

    for(i in 1:length(aseq)){

      for(j in 1:length(pseq)){

        error = try(AmiseNR(aseq[i],q,pseq[j],r,n),silent = TRUE)

        if(is.numeric(error)){

          errors[i,j] = error

        } else{

          errors[i,j] = Inf

        }
      }
    }

    best = which(errors == min(errors), arr.ind = TRUE)
    Opta = aseq[best[1]]
    Optp = pseq[best[2]]
    Opth = bw_ellip(Opta,q,Optp,r,n)

  }

  return(list("Opta" = Opta, "Optp" = Optp, "Opth" = Opth))

}

# Auxiliary functions

shrink = function(t,p,r){1-(1/(((r*t)^p) + 1))}

psifun = function(t,a,q){

  # Psi function, depending on parameter a

  -a + (a^(q/2) + t^(q/2))^(2/q)

}

psi = function(t,a,q){

  # Density of psifun(R^2) in case of a Gaussian generator

  (1/((2^((q/2)))*gamma(q/2))) * ((t+a)^((q/2)-1)) * exp((-1/2)*((((t+a)^(q/2)) - a^(q/2))^(2/q)))

}

sdpsi = function(t,a,q){

  # Second derivative of psi

  f1 = (1/((2^((q/2)+2))*gamma(q/2))) * ((t+a)^((q/2)-3))*exp((-1/2)*((((t+a)^(q/2)) - a^(q/2))^(2/q)))
  f2 = ((t+a)^q)*((((t+a)^(q/2))-a^(q/2))^((4/q)-2)) - 3*(q-2)*((t+a)^(q/2)) * ((((t+a)^(q/2))-a^(q/2))^((2/q)-1))
  f3 = (2-q)*((t+a)^q)*((((t+a)^(q/2))-a^(q/2))^((2/q)-2)) - (q-4)*(q-2)

  return(f1 * (f2-f3))

}

bw_ellip = function(a,q,p,r,n){

  # Optimal AMISE bandwidth for given a, q, p = delta, r and sample size n

  integrand1 = function(t,a,q,p,r){(shrink(t,p,r)^(-1)) * psi(t,a,q)}

  integrand2 = function(t,a,q,p,r){((shrink(t,p,r)^2) * sdpsi(t,a,q))^2}

  v1 = integrate(function(t,a,q,p,r){integrand1(t,a,q,p,r)}, lower = 0, upper = Inf,
                 a = a, q = q, p = p, r = r)$value

  v2 = integrate(function(t,a,q,p,r){integrand2(t,a,q,p,r)}, lower = 0, upper = Inf,
                 a = a, q = q, p = p, r = r)$value

  return(((((3/5) * v1)/(((1/5)^2)*v2))^(1/5)) * n^(-1/5))

}

I1 = function(a,q,p,r){

  # First integral for AMISE

  integrand = function(t,a,q,p,r){(shrink(psifun(t,a,q),p,r)^4) * (t^((q/2)-1)) * (((a^(q/2)) + (t^(q/2)))^(-8/q)) * exp(-t) *
      (((((a^(q/2)) + (t^(q/2)))^(2)) * t^(2-q) - 3*(q-2)*(((a^(q/2)) + (t^(q/2))))*(t^(1-(q/2))) -
          (2-q)*(((a^(q/2)) + (t^(q/2)))^(2))*(t^(1-q)) + (q-4)*(q-2))^2)}

  value = integrate(function(t,a,q,p,r){integrand(t,a,q,p,r)}, lower = 0, upper = Inf,
                    a = a, q = q, p = p, r = r)$value

  return(value)

}

I2 = function(a,q,p,r){

  # Second integral for AMISE

  integrand = function(t,a,q,p,r){(shrink(psifun(t,a,q),p,r)^(-1)) * (t^((q/2)-1)) * ((a^(q/2) + t^(q/2))^((2/q)-1))*exp(-t/2)}

  value = integrate(function(t,a,q,p,r){integrand(t,a,q,p,r)}, lower = 0, upper = Inf,
                    a = a, q = q, p = p, r = r)$value

  return(value)

}

AmiseNR = function(a,q,p,r,n){

  # Normal reference AMISE for identity correlation matrix

  bw = bw_ellip(a,q,p,r,n)
  t1 = ((bw^4)*I1(a,q,p,r))/(25 * (2^(q+6)) * (pi^(q/2)) * gamma(q/2))
  t2 = (3 * I2(a,q,p,r))/(5 * (2^(q/2)) * (pi^(q/2)) * n * bw)

  return(t1 + t2)

}
