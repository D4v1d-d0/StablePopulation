#' Reconstruct a stable Weibull survivorship profile
#'
#' @description
#' For a fixed Weibull shape parameter \eqn{\beta} and a fertility schedule
#' \eqn{m_x}, calculates the corresponding \eqn{\alpha} and reconstructs
#' \eqn{l_x} under the StablePopulation condition
#' \eqn{\sum_x l_xm_x = 1}.
#'
#' @details
#' Age classes are generated internally as \code{0, 1, 2, ..., n - 1}. The
#' function uses the verified internal solver used by the extended workflow; it
#' therefore stops rather than silently treating a search endpoint as a valid
#' root.
#'
#' @param fertility_rates Numeric vector of age-specific fertility rates.
#' @param beta Positive Weibull shape parameter.
#' @param tol Positive numerical tolerance for root finding.
#' @param r0_tolerance Positive tolerance used to assess numerical agreement
#'   with \eqn{R_0 = 1}.
#'
#' @return A list of class \code{"stable_population_reconstruction"} with the
#'   fitted parameters, the survivorship profile, a result table, and solver
#'   diagnostics.
#'
#' @examples
#' mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
#' reconstruction <- reconstruct_population(mx, beta = 1.10)
#' reconstruction$alpha
#' reconstruction$table
#'
#' @export
reconstruct_population <- function(
  fertility_rates,
  beta,
  tol = 1e-12,
  r0_tolerance = 1e-8
) {
  # Validate all inputs once.
  fertility_rates <- validate_fertility_rates(fertility_rates)
  beta <- validate_beta_values(beta)
  if (length(beta) != 1L) {
    stop(
      "'beta' must contain exactly one positive value.",
      call. = FALSE
    )
  }
  tol <- validate_positive_scalar(tol, "tol")
  r0_tolerance <- validate_positive_scalar(r0_tolerance, "r0_tolerance")

  # StablePopulation uses consecutive class indices, not an external age scale.
  age <- seq.int(0L, length(fertility_rates) - 1L)

  # Solve alpha under the R0 = 1 restriction.
  solver <- solve_alpha(
    beta = beta,
    fertility_rates = fertility_rates,
    tol = tol,
    r0_tolerance = r0_tolerance
  )

  if (!isTRUE(solver$converged)) {
    stop(
      "No valid alpha was found under the R0 = 1 constraint (status: ",
      solver$status, ").",
      call. = FALSE
    )
  }

  alpha <- solver$alpha
  lx <- weibull_survival(alpha = alpha, beta = beta, age = age)
  lx[1L] <- 1
  lxmx <- lx * fertility_rates
  R0 <- sum(lxmx)
  residual <- R0 - 1
  stable <- abs(residual) <= r0_tolerance

  # Guard against unexpected numerical inconsistencies.
  if (!stable) {
    stop(
      "The reconstructed profile did not satisfy the requested R0 tolerance.",
      call. = FALSE
    )
  }

  output_table <- data.frame(
    age = age,
    fertility_rates = fertility_rates,
    lx = lx,
    lxmx = lxmx,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      model = "weibull_R0_1",
      constrained_R0 = TRUE,
      age = age,
      fertility_rates = fertility_rates,
      beta = beta,
      alpha = alpha,
      lx = lx,
      population = lx,
      lxmx = lxmx,
      R0 = R0,
      births = R0,
      residual = residual,
      stable = stable,
      solver = solver,
      tolerance = list(root = tol, R0 = r0_tolerance),
      table = output_table
    ),
    class = "stable_population_reconstruction"
  )
}
