#' Evaluate the Stable-Stationary Constraint for Alpha
#'
#' Computes the difference between the reproductive output associated with a
#' candidate \eqn{\alpha} value and the stable-stationary target \eqn{R_0 = 1},
#' for a fixed \eqn{\beta} and fertility schedule.
#'
#' @param alpha A positive numeric value. Candidate Weibull scale parameter.
#' @param beta A positive numeric value. Weibull shape parameter.
#' @param fertility_rates A numeric vector of age-specific fertility values
#'   \eqn{m_x}.
#'
#' @return A numeric value equal to \eqn{\sum_x l_x(\alpha, \beta)m_x - 1}.
#'
#' @seealso \code{\link[stats]{uniroot}}, \code{\link{find_alphas}}
#'
#' @importFrom stats uniroot
#' @export
#'
#' @examples
#' alpha_objective(
#'   alpha = 5,
#'   beta = 1.2,
#'   fertility_rates = c(0, 0, 0.3, 0.7, 0.5)
#' )
alpha_objective <- function(alpha, beta, fertility_rates) {
  result <- calculate_population(alpha, beta, fertility_rates)
  result$births - 1
}


#' Find Alpha for a Fixed Beta Under R0 = 1
#'
#' Finds the Weibull scale parameter \eqn{\alpha} associated with a fixed shape
#' parameter \eqn{\beta}, using the stable-stationary constraint
#' \eqn{\sum_x l_x m_x = 1}.
#'
#' @param beta A positive numeric value. Weibull shape parameter.
#' @param fertility_rates A numeric vector of age-specific fertility values
#'   \eqn{m_x}.
#' @param tol A positive numeric value. Numerical tolerance passed to
#'   \code{\link[stats]{uniroot}}. Default is \code{1e-22}.
#'
#' @return A numeric value giving the \eqn{\alpha} value associated with the
#'   supplied \eqn{\beta} and fertility schedule under the stable-stationary
#'   constraint.
#'
#' @export
#'
#' @examples
#' find_alphas(
#'   beta = 1.2,
#'   fertility_rates = c(0, 0, 0.3, 0.7, 0.5)
#' )
find_alphas <- function(beta, fertility_rates, tol = 1e-22) {
  # Search interval for the Weibull scale parameter alpha.
  lower <- 1e-25
  upper <- 100000

  # Evaluate the stable-stationary constraint at both interval endpoints.
  f_lower <- alpha_objective(lower, beta, fertility_rates)
  f_upper <- alpha_objective(upper, beta, fertility_rates)

  # Use root finding when the interval brackets the stable-stationary target.
  if (f_lower * f_upper < 0) {
    root_result <- uniroot(
      alpha_objective,
      interval = c(lower, upper),
      beta = beta,
      fertility_rates = fertility_rates,
      tol = tol
    )
    return(root_result$root)
  }

  # Select the endpoint giving the closest value to the target within the
  # historical search interval.
  warning(
    "The objective function keeps the same sign over the search interval. ",
    "The returned alpha is the interval endpoint closest to the ",
    "stable-stationary target R0 = 1 within that interval. ",
    "Use reconstruct_population() for stricter reconstruction diagnostics.",
    call. = FALSE
  )

  if (abs(f_lower) < abs(f_upper)) {
    lower
  } else {
    upper
  }
}
