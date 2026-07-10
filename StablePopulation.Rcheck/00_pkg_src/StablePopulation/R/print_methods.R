#' Print a stable population reconstruction
#'
#' @param x An object of class \code{"stable_population_reconstruction"}.
#' @param ... Additional arguments (currently ignored).
#' @return \code{x}, invisibly.
#' @export
print.stable_population_reconstruction <- function(x, ...) {
  cat("Stable Population Reconstruction (Weibull, R0 = 1)\n")
  cat("---------------------------------------------------\n")
  cat("  alpha:", formatC(x$alpha, format = "fg", digits = 6), "\n")
  cat("  beta: ", formatC(x$beta, format = "fg", digits = 6), "\n")
  cat("  R0:   ", formatC(x$R0, format = "fg", digits = 8), "\n")
  cat("  Stable:", x$stable, "\n")
  cat("  Age classes:", length(x$age), "\n")
  cat("\nUse $table for the full age-specific profile.\n")
  invisible(x)
}

#' Print a stable population scan
#'
#' @param x An object of class \code{"stable_population_scan"}.
#' @param ... Additional arguments (currently ignored).
#' @return \code{x}, invisibly.
#' @export
print.stable_population_scan <- function(x, ...) {
  cat("Stable Population Scan (Weibull, R0 = 1)\n")
  cat("-----------------------------------------\n")
  cat("  Beta values scanned:", x$checks$n_beta, "\n")
  cat("  Numerically stable: ", x$checks$n_stable, "\n")
  cat("  Admissible:         ", x$checks$n_admissible, "\n")
  if (!is.null(x$terminal_window)) {
    cat("  Terminal window:     [",
        x$terminal_window[1], ", ", x$terminal_window[2], "]\n", sep = "")
  }
  cat("  Age classes:", length(x$age), "\n")
  cat("\nUse $summary for all candidates, $admissible_summary for retained ones.\n")
  invisible(x)
}

#' Print a stable population selection
#'
#' @param x An object of class \code{"stable_population_selection"}.
#' @param ... Additional arguments (currently ignored).
#' @return \code{x}, invisibly.
#' @export
print.stable_population_selection <- function(x, ...) {
  cat("Stable Population Selection (Weibull, R0 = 1, best RMSE)\n")
  cat("---------------------------------------------------------\n")
  cat("  Best beta: ", formatC(x$best_beta, format = "fg", digits = 6), "\n")
  cat("  Best alpha:", formatC(x$best_alpha, format = "fg", digits = 6), "\n")
  cat("  R0:        ", formatC(x$best_R0, format = "fg", digits = 8), "\n")
  cat("  RMSE:      ", formatC(x$best_RMSE, format = "g", digits = 4), "\n")
  cat("  Age classes:", length(x$age), "\n")
  if (isTRUE(x$beta_at_boundary)) {
    cat("  Boundary warning: selected beta is at the ",
        x$beta_boundary_side, " edge of the scanned range\n", sep = "")
  }
  n_candidates <- sum(x$results$stable)
  cat("  Candidates evaluated:", n_candidates, "\n")
  cat("\nUse $best_profile for the selected profile, $results for all candidates.\n")
  invisible(x)
}

#' Print a free Weibull fit
#'
#' @param x An object of class \code{"stable_population_free_fit"}.
#' @param ... Additional arguments (currently ignored).
#' @return \code{x}, invisibly.
#' @export
print.stable_population_free_fit <- function(x, ...) {
  cat("Free Weibull Fit (unconstrained, descriptive reference)\n")
  cat("-------------------------------------------------------\n")
  cat("  alpha:    ", formatC(x$alpha, format = "fg", digits = 6), "\n")
  cat("  beta:     ", formatC(x$beta, format = "fg", digits = 6), "\n")
  cat("  RMSE:     ", formatC(x$RMSE, format = "g", digits = 4), "\n")
  cat("  Converged:", x$converged, "\n")
  if (!is.na(x$R0_fitted)) {
    cat("  R0 (implied):", formatC(x$R0_fitted, format = "fg", digits = 6), "\n")
  }
  n_attempts <- nrow(x$attempts)
  n_converged <- sum(x$attempts$convergence == 0L, na.rm = TRUE)
  cat("  Optimization starts:", n_attempts, "(", n_converged, "converged )\n")
  cat("\nUse $profile for the fitted profile, $attempts for all starts.\n")
  invisible(x)
}

#' Print a demographic profile
#'
#' @param x An object of class \code{"stable_population_demographic_profile"}.
#' @param ... Additional arguments (currently ignored).
#' @return \code{x}, invisibly.
#' @export
print.stable_population_demographic_profile <- function(x, ...) {
  cat("Stable Population Demographic Profile\n")
  cat("--------------------------------------\n")
  cat("  Age classes:", length(x$age), "\n")
  if (!is.na(x$R0)) {
    cat("  R0:", formatC(x$R0, format = "fg", digits = 8), "\n")
    cat("  Stable R0:", x$stable_R0, "\n")
  }
  cat("  sum(R):", formatC(x$checks$stable_structure_sum, format = "fg", digits = 8), "\n")
  cat("  sum(D):", formatC(x$checks$mortality_profile_sum, format = "fg", digits = 8),
      " (expected:", formatC(x$checks$expected_mortality_profile_sum, format = "fg", digits = 8), ")\n")
  cat("  sum(D_relative):", formatC(x$checks$mortality_profile_relative_sum, format = "fg", digits = 8), "\n")
  cat("\nUse $table for the full profile with R, D, D_relative and B.\n")
  invisible(x)
}

#' Print a normalized fertility result
#'
#' @param x An object of class \code{"stable_population_normalized_fertility"}.
#' @param ... Additional arguments (currently ignored).
#' @return \code{x}, invisibly.
#' @export
print.stable_population_normalized_fertility <- function(x, ...) {
  cat("Normalized Fertility Schedule\n")
  cat("-----------------------------\n")
  cat("  R0 (reference):", formatC(x$R0_reference, format = "fg", digits = 6), "\n")
  cat("  Scaling factor:", formatC(x$factor, format = "fg", digits = 6), "\n")
  cat("  Check R0 (after normalization):", formatC(x$check_R0, format = "fg", digits = 8), "\n")
  cat("  Age classes:", length(x$fertility_rates), "\n")
  cat("\nUse $fertility_rates_normalized for the adjusted schedule.\n")
  invisible(x)
}
