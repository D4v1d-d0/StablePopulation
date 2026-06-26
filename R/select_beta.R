#' Select a constrained Weibull beta using observed survivorship
#'
#' @description
#' Generates Weibull survivorship candidates under \eqn{R_0 = 1} and selects
#' the candidate with the smallest RMSE against an observed survivorship profile
#' \eqn{l_x}. It is not a free two-parameter Weibull fit: \eqn{\alpha} remains
#' determined by the fertility schedule and the stability constraint for every
#' candidate \eqn{\beta}.
#'
#' @param fertility_rates Numeric vector of non-negative age-specific fertility
#'   rates.
#' @param lx_observed Numeric observed survivorship profile. It must have the
#'   same length as \code{fertility_rates}, start at one, and be non-increasing.
#' @param beta_values Positive Weibull shape values to scan.
#' @param tol Positive numerical tolerance for root finding.
#' @param r0_tolerance Positive tolerance for the numerical \eqn{R_0} check.
#' @param lx_tolerance Positive tolerance used to validate \code{lx_observed}.
#'
#' @return A list of class \code{"stable_population_selection"} containing the
#'   selected parameters, the selected profile, the full candidate table, and
#'   the underlying [scan_beta()] result.
#'
#' @examples
#' mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
#' lx_observed <- c(1, 0.93, 0.82, 0.67, 0.41, 0.15)
#'
#' selection <- select_beta(mx, lx_observed)
#' selection$best_beta
#' selection$best_profile
#'
#' @export
select_beta <- function(
  fertility_rates,
  lx_observed,
  beta_values = seq(0.05, 3.00, by = 0.05),
  tol = 1e-12,
  r0_tolerance = 1e-8,
  lx_tolerance = 1e-8
) {
  # Validate data before starting the scan.
  fertility_rates <- validate_fertility_rates(fertility_rates)
  lx_tolerance <- validate_positive_scalar(lx_tolerance, "lx_tolerance")
  lx_observed <- validate_lx_observed(
    lx_observed = lx_observed,
    expected_length = length(fertility_rates),
    tolerance = lx_tolerance
  )

  scan <- scan_beta(
    fertility_rates = fertility_rates,
    beta_values = beta_values,
    tol = tol,
    r0_tolerance = r0_tolerance,
    terminal_window = NULL
  )

  results <- scan$summary
  results$MSE <- NA_real_
  results$RMSE <- NA_real_
  results$selected <- FALSE

  # Compare only profiles that satisfy the requested R0 tolerance.
  stable_index <- which(results$stable)
  if (length(stable_index) == 0L) {
    stop(
      "No candidate beta produced a valid R0 = 1 profile.",
      call. = FALSE
    )
  }

  for (index in stable_index) {
    reconstructed <- as.numeric(scan$profiles[, index])
    residual <- reconstructed - lx_observed
    results$MSE[index] <- mean(residual ^ 2)
    results$RMSE[index] <- sqrt(results$MSE[index])
  }

  # beta_values are sorted, so which.min resolves exact RMSE ties toward lower beta.
  best_index <- stable_index[which.min(results$RMSE[stable_index])]
  results$selected[best_index] <- TRUE

  best_lx <- as.numeric(scan$profiles[, best_index])
  best_residual <- best_lx - lx_observed
  age <- scan$age

  best_profile <- data.frame(
    age = age,
    fertility_rates = fertility_rates,
    lx_observed = lx_observed,
    lx_reconstructed = best_lx,
    residual = best_residual,
    squared_error = best_residual ^ 2,
    lxmx = best_lx * fertility_rates,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      model = "weibull_R0_1_selected_by_RMSE",
      constrained_R0 = TRUE,
      age = age,
      fertility_rates = fertility_rates,
      lx_observed = lx_observed,
      best_beta = results$beta[best_index],
      best_alpha = results$alpha[best_index],
      best_lx = best_lx,
      best_R0 = results$R0[best_index],
      best_MSE = results$MSE[best_index],
      best_RMSE = results$RMSE[best_index],
      best_profile = best_profile,
      results = results,
      scan = scan,
      input = list(
        age = age,
        fertility_rates = fertility_rates,
        lx_observed = lx_observed
      ),
      tolerance = list(root = tol, R0 = r0_tolerance, lx = lx_tolerance)
    ),
    class = "stable_population_selection"
  )
}
