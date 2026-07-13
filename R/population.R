#' Compute a Weibull Survivorship Profile
#'
#' Computes the survivorship profile \eqn{l_x} for one or more ages using the
#' Weibull survival function defined by the scale parameter \eqn{\alpha} and
#' the shape parameter \eqn{\beta}.
#'
#' @param alpha A positive numeric value. Weibull scale parameter controlling
#'   the horizontal position of the survivorship curve.
#' @param beta A positive numeric value. Weibull shape parameter controlling
#'   the curvature of the survivorship profile.
#' @param age A numeric vector of ages or age-class indices.
#'
#' @return A numeric vector with the same length as \code{age}, giving the
#'   survivorship values \eqn{l_x}.
#'
#' @export
#'
#' @examples
#' weibull_survival(alpha = 5, beta = 1.2, age = 0:10)
weibull_survival <- function(alpha, beta, age) {
  exp(-((age / alpha)^beta))
}

#' Calculate a Normalized Survivorship Profile and Reproductive Output
#'
#' Computes the normalized survivorship profile associated with a Weibull curve
#' and combines it with an age-specific fertility schedule to obtain total
#' reproductive output.
#'
#' @param alpha A positive numeric value. Weibull scale parameter controlling
#'   the horizontal position of the survivorship curve.
#' @param beta A positive numeric value. Weibull shape parameter controlling
#'   the curvature of the survivorship profile.
#' @param fertility_rates A numeric vector of age-specific fertility values
#'   \eqn{m_x}. Its length defines the number of age classes.
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{population}{A numeric vector representing the normalized
#'       survivorship profile by age class. The first value is set to 1.}
#'     \item{births}{A numeric value equal to \eqn{\sum_x l_x m_x}, the
#'       reproductive output associated with the survivorship profile and
#'       fertility schedule.}
#'   }
#'
#' @export
#'
#' @examples
#' calculate_population(
#'   alpha = 5,
#'   beta = 1.2,
#'   fertility_rates = c(0, 0, 0.3, 0.7, 0.5)
#' )
calculate_population <- function(alpha, beta, fertility_rates) {
  # The fertility vector defines the number of age classes in the profile.
  periods <- length(fertility_rates)

  # The historical name "population" represents the normalized survivorship
  # profile used by the original StablePopulation workflow.
  population <- numeric(periods)
  population[1] <- 1

  # Fill the survivorship profile from age 1 to the final age class.
  for (i in 2:periods) {
    population[i] <- weibull_survival(alpha, beta, i - 1)
  }

  # Combine survivorship and fertility to obtain reproductive output.
  births <- sum(population * fertility_rates)

  list(population = population, births = births)
}
