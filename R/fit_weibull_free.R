#' Fit a free two-parameter Weibull survivorship model
#'
#' @description
#' Jointly estimates the Weibull scale parameter \eqn{\alpha} and shape
#' parameter \eqn{\beta} from an observed survivorship profile by minimizing
#' MSE. It does not impose \eqn{\sum_x l_xm_x = 1}.
#'
#' @details
#' This function is a reference comparison, not the default StablePopulation
#' reconstruction route. In contrast with [select_beta()], both parameters are
#' free. If \code{fertility_rates} are supplied, they are used only to report
#' the \eqn{R_0} implied by the fitted free profile.
#'
#' @param lx_observed Numeric observed survivorship profile.
#' @param fertility_rates Optional fertility schedule of the same length as
#'   \code{lx_observed}.
#' @param start_alpha Optional positive starting value for \eqn{\alpha}.
#' @param start_beta Optional positive starting value for \eqn{\beta}.
#' @param alpha_bounds Positive lower and upper bounds for \eqn{\alpha}.
#' @param beta_bounds Positive lower and upper bounds for \eqn{\beta}.
#' @param control Optional list passed to [stats::optim()].
#' @param lx_tolerance Positive tolerance used to validate \code{lx_observed}.
#'
#' @return A list of class \code{"stable_population_free_fit"} containing the
#'   free estimates, the fitted profile, error measures, all optimization starts,
#'   and \eqn{R_0} if fertility rates are supplied.
#'
#' @examples
#' lx_observed <- c(1, 0.92, 0.78, 0.57, 0.33, 0.12)
#' free_fit <- fit_weibull_free(lx_observed)
#' free_fit$alpha
#' free_fit$beta
#'
#' @export
fit_weibull_free <- function(
  lx_observed,
  fertility_rates = NULL,
  start_alpha = NULL,
  start_beta = NULL,
  alpha_bounds = c(1e-6, 1e6),
  beta_bounds = c(1e-4, 100),
  control = list(),
  lx_tolerance = 1e-8
) {
  # Validate the observed profile and optional fertility schedule.
  lx_tolerance <- validate_positive_scalar(lx_tolerance, "lx_tolerance")
  lx_observed <- validate_lx_observed(
    lx_observed = lx_observed,
    tolerance = lx_tolerance
  )

  if (!is.null(fertility_rates)) {
    fertility_rates <- validate_fertility_rates(fertility_rates)
    if (length(fertility_rates) != length(lx_observed)) {
      stop(
        "'fertility_rates' must have the same length as 'lx_observed'.",
        call. = FALSE
      )
    }
  }

  alpha_bounds <- validate_positive_bounds(alpha_bounds, "alpha_bounds")
  beta_bounds <- validate_positive_bounds(beta_bounds, "beta_bounds")

  if (!is.list(control)) {
    stop("'control' must be a list.", call. = FALSE)
  }

  if (!is.null(start_alpha)) {
    start_alpha <- validate_positive_scalar(start_alpha, "start_alpha")
  }
  if (!is.null(start_beta)) {
    start_beta <- validate_positive_scalar(start_beta, "start_beta")
  }

  age <- seq.int(0L, length(lx_observed) - 1L)

  # Build a data-driven default alpha start from the first l_x below exp(-1).
  crossing <- which(lx_observed[-1L] <= exp(-1))
  default_alpha <- if (length(crossing) > 0L) {
    age[crossing[1L] + 1L]
  } else {
    max(1, max(age))
  }
  default_alpha <- min(max(default_alpha, alpha_bounds[1L]), alpha_bounds[2L])

  # Multiple starts make the free reference fit less dependent on one initial guess.
  alpha_starts <- unique(c(
    default_alpha,
    sqrt(alpha_bounds[1L] * alpha_bounds[2L]),
    start_alpha
  ))
  alpha_starts <- alpha_starts[
    is.finite(alpha_starts) & alpha_starts >= alpha_bounds[1L] &
      alpha_starts <= alpha_bounds[2L]
  ]

  beta_starts <- unique(c(0.25, 0.50, 1, 1.50, 2, 3, start_beta))
  beta_starts <- beta_starts[
    is.finite(beta_starts) & beta_starts >= beta_bounds[1L] &
      beta_starts <= beta_bounds[2L]
  ]

  if (length(alpha_starts) == 0L || length(beta_starts) == 0L) {
    stop(
      "No valid optimization starts remained within the supplied bounds.",
      call. = FALSE
    )
  }

  starts <- expand.grid(
    start_alpha = alpha_starts,
    start_beta = beta_starts,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  # Optimize on the log scale to keep alpha and beta strictly positive.
  objective <- function(log_parameters) {
    alpha <- exp(log_parameters[1L])
    beta <- exp(log_parameters[2L])
    lx_fitted <- weibull_survival(alpha = alpha, beta = beta, age = age)
    mean((lx_fitted - lx_observed) ^ 2)
  }

  attempts <- vector("list", nrow(starts))
  for (index in seq_len(nrow(starts))) {
    start <- c(starts$start_alpha[index], starts$start_beta[index])

    fit <- tryCatch(
      stats::optim(
        par = log(start),
        fn = objective,
        method = "L-BFGS-B",
        lower = log(c(alpha_bounds[1L], beta_bounds[1L])),
        upper = log(c(alpha_bounds[2L], beta_bounds[2L])),
        control = control
      ),
      error = function(error) error
    )

    if (inherits(fit, "error")) {
      attempts[[index]] <- data.frame(
        start_alpha = start[1L],
        start_beta = start[2L],
        alpha = NA_real_,
        beta = NA_real_,
        MSE = NA_real_,
        RMSE = NA_real_,
        convergence = NA_integer_,
        status = paste0("optim_error: ", conditionMessage(fit)),
        stringsAsFactors = FALSE
      )
    } else {
      alpha <- exp(fit$par[1L])
      beta <- exp(fit$par[2L])
      mse <- as.numeric(fit$value)
      attempts[[index]] <- data.frame(
        start_alpha = start[1L],
        start_beta = start[2L],
        alpha = alpha,
        beta = beta,
        MSE = mse,
        RMSE = sqrt(mse),
        convergence = as.integer(fit$convergence),
        status = if (fit$convergence == 0L) "converged" else "optim_nonzero_convergence",
        stringsAsFactors = FALSE
      )
    }
  }

  attempts <- do.call(rbind, attempts)
  finite_attempts <- which(is.finite(attempts$MSE))
  if (length(finite_attempts) == 0L) {
    stop(
      "The free Weibull optimizer did not return a finite fit.",
      call. = FALSE
    )
  }

  # Prefer converged fits when available; otherwise retain the best finite attempt.
  converged_attempts <- finite_attempts[attempts$convergence[finite_attempts] == 0L]
  candidate_attempts <- if (length(converged_attempts) > 0L) {
    converged_attempts
  } else {
    finite_attempts
  }
  best_index <- candidate_attempts[which.min(attempts$MSE[candidate_attempts])]

  alpha <- attempts$alpha[best_index]
  beta <- attempts$beta[best_index]
  lx_fitted <- weibull_survival(alpha = alpha, beta = beta, age = age)
  residual <- lx_fitted - lx_observed
  mse <- mean(residual ^ 2)
  rmse <- sqrt(mse)

  if (is.null(fertility_rates)) {
    lxmx <- rep(NA_real_, length(age))
    R0_fitted <- NA_real_
  } else {
    lxmx <- lx_fitted * fertility_rates
    R0_fitted <- sum(lxmx)
  }

  profile <- data.frame(
    age = age,
    lx_observed = lx_observed,
    lx_fitted = lx_fitted,
    residual = residual,
    squared_error = residual ^ 2,
    fertility_rates = if (is.null(fertility_rates)) rep(NA_real_, length(age)) else fertility_rates,
    lxmx = lxmx,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      model = "weibull_free",
      constrained_R0 = FALSE,
      age = age,
      lx_observed = lx_observed,
      fertility_rates = fertility_rates,
      alpha = alpha,
      beta = beta,
      lx_fitted = lx_fitted,
      R0_fitted = R0_fitted,
      MSE = mse,
      RMSE = rmse,
      converged = attempts$convergence[best_index] == 0L,
      selected_attempt = best_index,
      attempts = attempts,
      bounds = list(alpha = alpha_bounds, beta = beta_bounds),
      profile = profile,
      tolerance = list(lx = lx_tolerance)
    ),
    class = "stable_population_free_fit"
  )
}
