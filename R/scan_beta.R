#' Scan Weibull beta values under the stable-population constraint
#'
#' @description
#' For every candidate \eqn{\beta}, calculates the corresponding \eqn{\alpha},
#' reconstructs a Weibull survivorship profile \eqn{l_x}, and checks whether the
#' numerical result satisfies \eqn{R_0 = \sum_x l_xm_x = 1}.
#'
#' @details
#' Age classes are always assigned internally as \code{0, 1, 2, ..., n - 1}.
#' When \code{terminal_window} is supplied, the function marks the profiles
#' whose final survivorship lies inside that inclusive window. This scenario
#' route is useful when an observed \eqn{l_x} is not available; it does not
#' select a single empirical optimum.
#'
#' @param fertility_rates Numeric vector of non-negative age-specific fertility
#'   rates.
#' @param beta_values Positive Weibull shape values to scan.
#' @param tol Positive numerical tolerance for root finding.
#' @param r0_tolerance Positive tolerance for the numerical \eqn{R_0} check.
#' @param terminal_window Optional numeric vector \code{c(lower, upper)} in
#'   \code{[0, 1]}. A stable profile is terminally admissible when its final
#'   \eqn{l_x} lies in this inclusive interval. Default: \code{NULL}.
#'
#' @return A list of class \code{"stable_population_scan"}. Its main elements
#'   are \code{summary}, \code{profiles}, \code{admissible_summary},
#'   \code{profiles_admissible}, and, when a terminal window is active,
#'   \code{terminal_extremes}.
#'
#' @examples
#' mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
#'
#' # All stable candidate profiles
#' scan <- scan_beta(mx, beta_values = seq(0.05, 1.50, by = 0.05))
#'
#' # Scenario route without observed lx
#' terminal_scan <- scan_beta(
#'   mx,
#'   beta_values = seq(0.05, 1.50, by = 0.05),
#'   terminal_window = c(1e-4, 0.05)
#' )
#' terminal_scan$admissible_summary
#'
#' @export
scan_beta <- function(
  fertility_rates,
  beta_values = seq(0.05, 3.00, by = 0.05),
  tol = 1e-12,
  r0_tolerance = 1e-8,
  terminal_window = NULL
) {
  # Validate inputs once and reuse the validated objects throughout the scan.
  fertility_rates <- validate_fertility_rates(fertility_rates)
  beta_values <- validate_beta_values(beta_values)
  tol <- validate_positive_scalar(tol, "tol")
  r0_tolerance <- validate_positive_scalar(r0_tolerance, "r0_tolerance")
  terminal_window <- validate_terminal_window(terminal_window)

  age <- seq.int(0L, length(fertility_rates) - 1L)
  n_age <- length(age)
  n_beta <- length(beta_values)

  # Use readable and unique profile column names.
  beta_labels <- make.unique(paste0(
    "beta_",
    formatC(beta_values, format = "fg", digits = 10, flag = "#")
  ))

  profiles <- matrix(
    NA_real_,
    nrow = n_age,
    ncol = n_beta,
    dimnames = list(paste0("age_", age), beta_labels)
  )

  summary_table <- data.frame(
    beta = beta_values,
    alpha = rep(NA_real_, n_beta),
    R0 = rep(NA_real_, n_beta),
    residual = rep(NA_real_, n_beta),
    lx_terminal = rep(NA_real_, n_beta),
    stable = rep(FALSE, n_beta),
    terminal_admissible = rep(NA, n_beta),
    admissible = rep(FALSE, n_beta),
    status = rep(NA_character_, n_beta),
    stringsAsFactors = FALSE
  )

  # Build every candidate profile. A failed beta is recorded rather than fatal.
  for (index in seq_along(beta_values)) {
    beta <- beta_values[index]

    solution <- tryCatch(
      solve_alpha(
        beta = beta,
        fertility_rates = fertility_rates,
        tol = tol,
        r0_tolerance = r0_tolerance
      ),
      error = function(error) list(
        alpha = NA_real_, R0 = NA_real_, residual = NA_real_,
        converged = FALSE, status = paste0("solver_error: ", conditionMessage(error)),
        bracket = c(NA_real_, NA_real_), beta = beta
      )
    )

    summary_table$alpha[index] <- solution$alpha
    summary_table$R0[index] <- solution$R0
    summary_table$residual[index] <- solution$residual
    summary_table$stable[index] <- isTRUE(solution$converged)
    summary_table$status[index] <- solution$status

    if (isTRUE(solution$converged)) {
      lx <- weibull_survival(alpha = solution$alpha, beta = beta, age = age)
      lx[1L] <- 1
      profiles[, index] <- lx
      summary_table$lx_terminal[index] <- lx[n_age]
    }
  }

  # With no terminal window, every numerically stable profile is admissible.
  if (is.null(terminal_window)) {
    summary_table$admissible <- summary_table$stable
  } else {
    summary_table$terminal_admissible <-
      summary_table$stable &
      summary_table$lx_terminal >= terminal_window[1L] &
      summary_table$lx_terminal <= terminal_window[2L]
    summary_table$admissible <- summary_table$terminal_admissible
  }

  admissible_index <- which(summary_table$admissible)
  profiles_admissible <- profiles[, admissible_index, drop = FALSE]
  admissible_summary <- summary_table[admissible_index, , drop = FALSE]

  # Build the first and last admissible profiles in beta order, matching MATLAB.
  build_terminal_candidate <- function(index) {
    list(
      beta = summary_table$beta[index],
      alpha = summary_table$alpha[index],
      R0 = summary_table$R0[index],
      lx_terminal = summary_table$lx_terminal[index],
      lx = as.numeric(profiles[, index]),
      profile = data.frame(
        age = age,
        fertility_rates = fertility_rates,
        lx = as.numeric(profiles[, index]),
        stringsAsFactors = FALSE
      )
    )
  }

  terminal_extremes <- NULL
  if (!is.null(terminal_window) && length(admissible_index) > 0L) {
    terminal_extremes <- list(
      first = build_terminal_candidate(admissible_index[1L]),
      last = build_terminal_candidate(admissible_index[length(admissible_index)])
    )
  }

  # Make an empty terminal selection explicit rather than silently ambiguous.
  if (!is.null(terminal_window) && length(admissible_index) == 0L) {
    warning(
      "No stable profile satisfies the terminal window; revise the age range, beta range, or terminal criterion.",
      call. = FALSE
    )
  }

  structure(
    list(
      model = "weibull_R0_1_scan",
      constrained_R0 = TRUE,
      age = age,
      fertility_rates = fertility_rates,
      beta_values = beta_values,
      terminal_window = terminal_window,
      summary = summary_table,
      profiles = profiles,
      admissible_summary = admissible_summary,
      profiles_admissible = profiles_admissible,
      terminal_extremes = terminal_extremes,
      checks = list(
        n_beta = n_beta,
        n_stable = sum(summary_table$stable),
        n_admissible = length(admissible_index)
      ),
      tolerance = list(root = tol, R0 = r0_tolerance)
    ),
    class = "stable_population_scan"
  )
}
