#' Validate age-specific fertility rates
#'
#' @description
#' Checks that an age-specific fertility schedule is a finite, non-negative,
#' numeric vector with at least one positive value. StablePopulation assigns
#' age classes internally as consecutive indices \code{0, 1, 2, ...}.
#'
#' @param fertility_rates Numeric vector of non-negative age-specific fertility
#'   rates.
#' @param min_length Minimum permitted number of age classes.
#'
#' @return A validated numeric vector.
#'
#' @keywords internal
validate_fertility_rates <- function(fertility_rates, min_length = 2L) {
  # Validate the requested minimum length.
  if (!is.numeric(min_length) || length(min_length) != 1L ||
      !is.finite(min_length) || min_length < 1 ||
      min_length != as.integer(min_length)) {
    stop(
      "'min_length' must be one positive whole number.",
      call. = FALSE
    )
  }
  min_length <- as.integer(min_length)

  # Require a plain numeric vector, not a matrix or data frame.
  if (!is.numeric(fertility_rates) || !is.atomic(fertility_rates) ||
      !is.null(dim(fertility_rates))) {
    stop(
      "'fertility_rates' must be a numeric vector.",
      call. = FALSE
    )
  }

  if (length(fertility_rates) < min_length) {
    stop(
      "'fertility_rates' must contain at least ", min_length,
      " age classes.",
      call. = FALSE
    )
  }

  # Reject NA, NaN, Inf, and -Inf.
  if (any(!is.finite(fertility_rates))) {
    stop(
      "'fertility_rates' cannot contain NA, NaN, or infinite values.",
      call. = FALSE
    )
  }

  if (any(fertility_rates < 0)) {
    stop(
      "'fertility_rates' cannot contain negative values.",
      call. = FALSE
    )
  }

  if (!any(fertility_rates > 0)) {
    stop(
      "'fertility_rates' must contain at least one positive value.",
      call. = FALSE
    )
  }

  # Coerce consistently while retaining supplied names.
  supplied_names <- names(fertility_rates)
  fertility_rates <- as.numeric(fertility_rates)
  if (!is.null(supplied_names)) {
    names(fertility_rates) <- supplied_names
  }

  fertility_rates
}

#' Validate an observed or reconstructed survivorship profile
#'
#' @description
#' Checks that a survivorship vector \eqn{l_x} is finite, one-dimensional,
#' bounded between zero and one, starts at one, and is non-increasing with age.
#' Tiny rounding deviations within \code{tolerance} are accepted and then
#' clipped to the interval \code{[0, 1]}.
#'
#' @param lx_observed Numeric vector of survivorship values.
#' @param expected_length Optional expected length.
#' @param tolerance Positive numeric tolerance for validation.
#' @param min_length Minimum permitted number of age classes.
#'
#' @return A validated numeric survivorship vector.
#'
#' @keywords internal
validate_lx_observed <- function(
  lx_observed,
  expected_length = NULL,
  tolerance = 1e-8,
  min_length = 2L
) {
  # Validate tolerance and requested lengths before using them.
  tolerance <- validate_positive_scalar(tolerance, "tolerance")

  if (!is.numeric(min_length) || length(min_length) != 1L ||
      !is.finite(min_length) || min_length < 1 ||
      min_length != as.integer(min_length)) {
    stop(
      "'min_length' must be one positive whole number.",
      call. = FALSE
    )
  }
  min_length <- as.integer(min_length)

  if (!is.null(expected_length)) {
    if (!is.numeric(expected_length) || length(expected_length) != 1L ||
        !is.finite(expected_length) || expected_length < min_length ||
        expected_length != as.integer(expected_length)) {
      stop(
        "'expected_length' must be NULL or one whole number no smaller than ",
        min_length, ".",
        call. = FALSE
      )
    }
    expected_length <- as.integer(expected_length)
  }

  # Require a plain numeric vector, not a matrix or data frame.
  if (!is.numeric(lx_observed) || !is.atomic(lx_observed) ||
      !is.null(dim(lx_observed))) {
    stop(
      "'lx_observed' must be a numeric vector.",
      call. = FALSE
    )
  }

  if (length(lx_observed) < min_length) {
    stop(
      "'lx_observed' must contain at least ", min_length,
      " age classes.",
      call. = FALSE
    )
  }

  if (!is.null(expected_length) && length(lx_observed) != expected_length) {
    stop(
      "'lx_observed' must have length ", expected_length, ".",
      call. = FALSE
    )
  }

  if (any(!is.finite(lx_observed))) {
    stop(
      "'lx_observed' cannot contain NA, NaN, or infinite values.",
      call. = FALSE
    )
  }

  if (any(lx_observed < -tolerance) || any(lx_observed > 1 + tolerance)) {
    stop(
      "All values in 'lx_observed' must be between 0 and 1.",
      call. = FALSE
    )
  }

  if (abs(lx_observed[1L] - 1) > tolerance) {
    stop(
      "The first value of 'lx_observed' must be 1.",
      call. = FALSE
    )
  }

  # Survivorship can stay constant or decrease, but cannot increase.
  if (any(diff(lx_observed) > tolerance)) {
    stop(
      "'lx_observed' must be non-increasing with age.",
      call. = FALSE
    )
  }

  supplied_names <- names(lx_observed)
  lx_observed <- pmin(pmax(as.numeric(lx_observed), 0), 1)
  if (!is.null(supplied_names)) {
    names(lx_observed) <- supplied_names
  }

  lx_observed
}

# Validate a vector of Weibull beta values.
# This helper is internal and intentionally not exported.
validate_beta_values <- function(beta_values) {
  if (!is.numeric(beta_values) || !is.atomic(beta_values) ||
      !is.null(dim(beta_values)) || length(beta_values) == 0L ||
      any(!is.finite(beta_values))) {
    stop(
      "'beta_values' must be a non-empty numeric vector of finite values.",
      call. = FALSE
    )
  }

  if (any(beta_values <= 0)) {
    stop(
      "All values in 'beta_values' must be strictly positive.",
      call. = FALSE
    )
  }

  sort(unique(as.numeric(beta_values)))
}

# Validate a positive scalar.
# This helper is internal and intentionally not exported.
validate_positive_scalar <- function(value, argument_name) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      value <= 0) {
    stop(
      "'", argument_name, "' must be one positive finite numeric value.",
      call. = FALSE
    )
  }

  as.numeric(value)
}

# Validate an optional terminal survivorship window.
# This helper is internal and intentionally not exported.
validate_terminal_window <- function(terminal_window) {
  if (is.null(terminal_window)) {
    return(NULL)
  }

  if (!is.numeric(terminal_window) || length(terminal_window) != 2L ||
      any(!is.finite(terminal_window)) || terminal_window[1L] < 0 ||
      terminal_window[2L] > 1 || terminal_window[1L] > terminal_window[2L]) {
    stop(
      "'terminal_window' must be NULL or two finite values in [0, 1] in ",
      "non-decreasing order.",
      call. = FALSE
    )
  }

  as.numeric(terminal_window)
}

# Validate lower and upper optimization bounds.
# This helper is internal and intentionally not exported.
validate_positive_bounds <- function(bounds, argument_name) {
  if (!is.numeric(bounds) || length(bounds) != 2L ||
      any(!is.finite(bounds)) || any(bounds <= 0) || bounds[1L] >= bounds[2L]) {
    stop(
      "'", argument_name, "' must contain two finite positive values with ",
      "lower < upper.",
      call. = FALSE
    )
  }

  as.numeric(bounds)
}
