#' Normalize fertility using a reference survivorship profile
#'
#' @description
#' Divides an age-specific fertility schedule by the net replacement calculated
#' from a reference survivorship profile. The returned schedule satisfies
#' \eqn{\sum_x l_x m_x = 1} for that reference profile.
#'
#' @details
#' This helper makes fertility rescaling explicit before data enter a route that
#' fixes \eqn{R_0 = 1}. It multiplies every fertility value by the same factor,
#' preserving the age pattern while changing the schedule's overall scale.
#'
#' It does not silently correct observed data or alter the package's internal
#' demographic rule. Use it when the scientific analysis is deliberately framed
#' at \eqn{R_0 = 1} relative to a known reference survivorship profile. Raw and
#' normalized schedules should be treated as distinct analytical inputs.
#'
#' @param fertility_rates Numeric vector of age-specific fertility rates.
#' @param lx_reference Numeric reference survivorship profile. It must have the
#'   same length as \code{fertility_rates}.
#' @param tolerance Positive numerical tolerance used to validate
#'   \code{lx_reference}.
#'
#' @return A list of class \code{"stable_population_normalized_fertility"}
#'   containing the original \code{fertility_rates},
#'   \code{fertility_rates_normalized}, \code{R0_reference}, the scaling
#'   \code{factor}, and the numerical \code{check_R0}.
#'
#' @seealso [select_beta()] for the observed-survivorship route and
#'   [run_reconstruction_excel()] for workbook analysis. The Excel workflow
#'   uses supplied fertility values as-is, so normalized values must be prepared
#'   before saving them to a workbook.
#'
#' @examples
#' mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
#' lx <- c(1, 0.90, 0.75, 0.55, 0.30, 0.10)
#' normalized <- normalize_fertility(mx, lx)
#' normalized$check_R0
#'
#' @export
normalize_fertility <- function(
  fertility_rates,
  lx_reference,
  tolerance = 1e-8
) {
  # Validate both schedules before calculating the reference replacement.
  fertility_rates <- validate_fertility_rates(fertility_rates)
  tolerance <- validate_positive_scalar(tolerance, "tolerance")
  lx_reference <- validate_lx_observed(
    lx_observed = lx_reference,
    expected_length = length(fertility_rates),
    tolerance = tolerance
  )

  R0_reference <- sum(lx_reference * fertility_rates)
  if (!is.finite(R0_reference) || R0_reference <= 0) {
    stop(
      "Reference R0 must be positive to normalize fertility.",
      call. = FALSE
    )
  }

  fertility_rates_normalized <- fertility_rates / R0_reference
  check_R0 <- sum(lx_reference * fertility_rates_normalized)

  structure(
    list(
      fertility_rates = fertility_rates,
      lx_reference = lx_reference,
      fertility_rates_normalized = fertility_rates_normalized,
      R0_reference = R0_reference,
      factor = 1 / R0_reference,
      check_R0 = check_R0,
      tolerance = tolerance
    ),
    class = "stable_population_normalized_fertility"
  )
}
