#' Derive stable-structure and mortality profiles from survivorship
#'
#' @description
#' Converts a survivorship profile \eqn{l_x} into the quantities used by the
#' demographic-to-ecological bridge in Model-V2022: the normalized stable
#' structure \eqn{R_x}, the exit-by-death profile \eqn{D_x}, its relative
#' version, and conditional survival between consecutive classes.
#'
#' @details
#' The stable structure is \eqn{R_x = l_x / \sum_x l_x}. The mortality profile
#' is \eqn{D_x = R_x - R_{x+1}} for all but the last class, and
#' \eqn{D_n = R_n}. Its total is \eqn{R_{x=0}} of the stable structure, not
#' necessarily one. The relative profile is \eqn{D_x / \sum_x D_x}; it is
#' therefore a distinct quantity that sums to one.
#'
#' @param lx Numeric survivorship profile. It must start at one and be
#'   non-increasing.
#' @param fertility_rates Optional fertility schedule with the same length as
#'   \code{lx}. When supplied, the result also includes \eqn{l_xm_x} and
#'   \eqn{R_0}.
#' @param tolerance Positive tolerance used to validate \code{lx} and assess
#'   \eqn{R_0 = 1} when fertility is provided.
#'
#' @return A list of class \code{"stable_population_demographic_profile"}.
#'
#' @examples
#' mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
#' reconstruction <- reconstruct_population(mx, beta = 1.10)
#'
#' demographic_profile <- derive_demographic_profile(
#'   lx = reconstruction$lx,
#'   fertility_rates = mx
#' )
#'
#' demographic_profile$table
#'
#' @export
derive_demographic_profile <- function(
  lx,
  fertility_rates = NULL,
  tolerance = 1e-8
) {
  # Reuse the same checks for observed and reconstructed survivorship vectors.
  tolerance <- validate_positive_scalar(tolerance, "tolerance")
  lx <- validate_lx_observed(lx_observed = lx, tolerance = tolerance)

  age <- seq.int(0L, length(lx) - 1L)
  n_age <- length(lx)

  if (!is.null(fertility_rates)) {
    fertility_rates <- validate_fertility_rates(fertility_rates)
    if (length(fertility_rates) != n_age) {
      stop(
        "'fertility_rates' must have the same length as 'lx'.",
        call. = FALSE
      )
    }
  }

  # R is the stable or stationary age structure used by the MATLAB workflow.
  stable_structure <- lx / sum(lx)

  # D is the raw exit-by-death profile. Appending zero implements D_n = R_n.
  # Its total equals R at age 0, so it is not generally a probability vector.
  mortality_profile <- -diff(c(stable_structure, 0))

  # The relative profile is a distinct, explicitly normalized quantity.
  mortality_total <- sum(mortality_profile)
  mortality_profile_relative <- mortality_profile / mortality_total

  # B_x is survival from age x to the next consecutive class. Once lx is zero,
  # conditional survival is undefined rather than forced to 0/0.
  conditional_survival <- rep(NA_real_, n_age)
  denominator <- lx[-n_age]
  numerator <- lx[-1L]
  defined_survival <- which(denominator > 0)
  conditional_survival[defined_survival] <-
    numerator[defined_survival] / denominator[defined_survival]

  if (is.null(fertility_rates)) {
    fertility_column <- rep(NA_real_, n_age)
    lxmx <- rep(NA_real_, n_age)
    R0 <- NA_real_
    stable_R0 <- NA
  } else {
    fertility_column <- fertility_rates
    lxmx <- lx * fertility_rates
    R0 <- sum(lxmx)
    stable_R0 <- abs(R0 - 1) <= tolerance
  }

  output_table <- data.frame(
    age = age,
    fertility_rates = fertility_column,
    lx = lx,
    stable_structure = stable_structure,
    mortality_profile = mortality_profile,
    mortality_profile_relative = mortality_profile_relative,
    conditional_survival = conditional_survival,
    lxmx = lxmx,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      age = age,
      fertility_rates = fertility_rates,
      lx = lx,
      stable_structure = stable_structure,
      R = stable_structure,
      mortality_profile = mortality_profile,
      D = mortality_profile,
      mortality_profile_relative = mortality_profile_relative,
      D_relative = mortality_profile_relative,
      conditional_survival = conditional_survival,
      B = conditional_survival,
      lxmx = lxmx,
      R0 = R0,
      stable_R0 = stable_R0,
      checks = list(
        stable_structure_sum = sum(stable_structure),
        mortality_profile_sum = mortality_total,
        expected_mortality_profile_sum = stable_structure[1L],
        mortality_profile_relative_sum = sum(mortality_profile_relative)
      ),
      table = output_table
    ),
    class = "stable_population_demographic_profile"
  )
}
