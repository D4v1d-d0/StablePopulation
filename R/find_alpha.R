#' Objective function for `uniroot`: finds the difference between births and 1
#'
#' This function calculates the difference between the number of births calculated with the given values of alpha, beta, and fertility rates, and the target value of 1.
#' @importFrom stats uniroot
#' @param alpha A numeric value representing the alpha parameter.
#' @param beta A numeric value representing the beta parameter.
#' @param fertility_rates A numeric vector containing the fertility rates.
#' @return The difference between the number of births and 1.
#' @export
#' @examples
#' alpha_objective(0.5, 1.2, c(0.2, 0.3, 0.5, 0.4))
alpha_objective <- function(alpha, beta, fertility_rates) {
  result <- calculate_population(alpha, beta, fertility_rates)
  return(result$births - 1)  # The difference from the target of 1
}

#' Function to find the value of alpha
#'
#' This function finds the value of alpha using the `uniroot` method for a given beta and a vector of fertility rates. If the function values at the interval ends do not have opposite signs, it returns the closest value to 0.
#' @param beta A numeric value representing the beta parameter.
#' @param fertility_rates A numeric vector containing the fertility rates.
#' @param tol Tolerance for the `uniroot` method. Default is `1e-22`.
#' @return The value of alpha found by `uniroot`, or the closest endpoint to 0 if opposite signs are not found.
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
