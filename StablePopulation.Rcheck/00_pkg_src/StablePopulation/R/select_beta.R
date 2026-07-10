#' Select a constrained Weibull beta using observed survivorship
#'
#' @description
#' Generates Weibull survivorship candidates under \eqn{R_0 = 1} and selects
#' the candidate with the smallest RMSE against an observed survivorship profile
#' \eqn{l_x}. It is not a free two-parameter Weibull fit: \eqn{\alpha} remains
#' determined by the fertility schedule and the stability constraint for every
#' candidate \eqn{\beta}.
#'
#' @details
#' This is the route to use when observed survivorship is available. The
#' function scans the supplied beta grid and returns the best grid candidate;
#' inspect \code{results} rather than treating the selected value as a
#' continuous unconstrained optimum. If the selected candidate lies at the
#' lower or upper edge of the scanned beta grid, the function issues a warning
#' and records a boundary note in the returned object, because the best value
#' may lie outside the supplied range.
#'
#' Fertility is analysed exactly as supplied. The function never rescales it
#' implicitly. When the methodological decision is to express an observed
#' fertility schedule relative to the stable condition, call
#' [normalize_fertility()] first and supply
#' \code{fertility_rates_normalized}. Using raw fertility and using an
#' explicitly normalised schedule are different analyses and should remain
#' distinguishable.
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
#' @return A list of class \code{"stable_population_selection"}. Main
#'   elements are \code{best_beta}, \code{best_alpha}, \code{best_lx},
#'   \code{best_R0}, \code{best_MSE}, \code{best_RMSE},
#'   \code{best_profile} (age-by-age comparison), \code{results} (all
#'   scanned candidates), \code{beta_at_boundary},
#'   \code{beta_boundary_side}, \code{beta_boundary_note}, and the
#'   underlying \code{scan} object.
#'
#' @seealso [scan_beta()] for the route without observed survivorship,
#'   [normalize_fertility()] for explicit fertility rescaling, and
#'   [fit_weibull_free()] for an unconstrained reference fit.
#'
#' @examples
#' mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
#' lx_observed <- c(1, 0.8302461, 0.6893086, 0.5722958, 0.4751464, 0.3944885)
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

  beta_range <- range(scan$beta_values)
  best_beta <- results$beta[best_index]
  beta_boundary_side <- NA_character_
  beta_boundary_note <- NA_character_

  if (length(scan$beta_values) > 1L && identical(best_beta, beta_range[1L])) {
    beta_boundary_side <- "lower"
  }
  if (length(scan$beta_values) > 1L && identical(best_beta, beta_range[2L])) {
    beta_boundary_side <- if (is.na(beta_boundary_side)) {
      "upper"
    } else {
      "lower and upper"
    }
  }

  beta_at_boundary <- !is.na(beta_boundary_side)
  if (beta_at_boundary) {
    beta_boundary_note <- paste0(
      "The selected beta (",
      formatC(best_beta, format = "fg", digits = 6),
      ") is at the ", beta_boundary_side,
      " boundary of the scanned beta range [",
      formatC(beta_range[1L], format = "fg", digits = 6),
      ", ",
      formatC(beta_range[2L], format = "fg", digits = 6),
      "]. Consider extending 'beta_values' and rerunning the selection."
    )
    warning(beta_boundary_note, call. = FALSE)
  }

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
      best_beta = best_beta,
      best_alpha = results$alpha[best_index],
      best_lx = best_lx,
      best_R0 = results$R0[best_index],
      best_MSE = results$MSE[best_index],
      best_RMSE = results$RMSE[best_index],
      best_profile = best_profile,
      results = results,
      beta_at_boundary = beta_at_boundary,
      beta_boundary_side = beta_boundary_side,
      beta_boundary_note = beta_boundary_note,
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
