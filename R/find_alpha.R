#' Objective Function for \code{uniroot}: Finds the Difference Between Births and 1
#'
#' This function calculates the difference between the number of births, as calculated with the given values
#' of \code{alpha}, \code{beta}, and \code{fertility_rates}, and the target value of 1.
#'
#' Typically used as the objective function in root-finding algorithms such as [uniroot][stats::uniroot], to determine
#' the value of alpha that results in exactly one birth.
#'
#' This function depends on [calculate_population], which must be available in your package namespace.
#'
#' @param alpha A numeric value representing the alpha parameter.
#' @param beta A numeric value representing the beta parameter.
#' @param fertility_rates A numeric vector containing the fertility rates.
#'
#' @return A numeric value giving the difference between the number of births (as calculated) and 1.
#'
#' @seealso \code{\link[stats]{uniroot}}
#'
#' @importFrom stats uniroot
#' @export
#'
#' @examples
#' # Basic usage
#' alpha_objective(0.5, 1.2, c(0.2, 0.3, 0.5, 0.4))
#'
#' # Example with uniroot:
#' fertility_rates <- c(0.2, 0.3, 0.5, 0.4)
#' beta <- 1.2
#' res <- uniroot(
#'   alpha_objective,
#'   interval = c(0.000001, 100),
#'   beta = beta,
#'   fertility_rates = fertility_rates
#' )
#' res$root
alpha_objective <- function(alpha, beta, fertility_rates) {
  result <- calculate_population(alpha, beta, fertility_rates)
  result$births - 1  # The difference from the target of 1
}


#' Function to find the value of alpha
#'
#' This function finds the value of alpha using the \code{uniroot} method for a given beta and a vector
#' of fertility rates. If the function values at the interval ends do not have opposite signs,
#' it returns the closest value to 0.
#'
#' @param beta A numeric value representing the beta parameter of Weibull distribution.
#' @param fertility_rates A numeric vector containing the fertility rates.
#' @param tol A numeric value representing the tolerance for the \code{uniroot} method. Default is \code{1e-22}.
#' @return A numeric value giving the estimated value of alpha, either found by \code{uniroot} or selected as the endpoint closest to zero if the root is not bracketed.
#' @export
#' @examples
#' find_alphas(1.2, c(0.2, 0.3, 0.5, 0.4))
find_alphas <- function(beta, fertility_rates, tol = 1e-22) {
  # Define the range to search for alpha
  lower <- 1e-25
  upper <- 100000

  # Evaluate the function at the interval endpoints
  f_lower <- alpha_objective(lower, beta, fertility_rates)
  f_upper <- alpha_objective(upper, beta, fertility_rates)

  # Check if the signs are opposite
  if (f_lower * f_upper < 0) {
    # If the signs are opposite, use uniroot
    resulta <- uniroot(alpha_objective, interval = c(lower, upper), beta = beta,
                       fertility_rates = fertility_rates, tol = tol)
    return(resulta$root)  # Return the alpha value found
  } else {
    # If the signs are the same, return the value closest to 0
    if (abs(f_lower) < abs(f_upper)) {
      return(lower)  # f(lower) is closer to 0
    } else {
      return(upper)  # f(upper) is closer to 0
    }
  }
}
