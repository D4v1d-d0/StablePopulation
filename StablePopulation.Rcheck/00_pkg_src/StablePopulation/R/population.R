#' Weibull function for the survival rate
#'
#' This function calculates the survival rate to reach a specific age using the Weibull function.
#'
#' @param alpha A numeric value representing the scale parameter of the Weibull distribution.
#' @param beta A numeric value representing the shape parameter of the Weibull distribution.
#' @param age A numeric value representing the age.
#' @return A numeric value giving the survival rate (probability) for reaching the given age.
#' @export
#' @examples
#' weibull_survival(1.5, 0.8, 10)
weibull_survival <- function(alpha, beta, age) {
  exp(-((age / alpha)^beta))
}

#' Calculates the population for each age group
#'
#' This function calculates the population for each age group and the number of births.
#'
#' @param alpha A numeric value representing the scale parameter (\eqn{\alpha}) of the Weibull distribution. Note: In this context, alpha controls the horizontal scaling of the survival curve.
#' @param beta A numeric value representing the shape parameter (\eqn{\beta}) of the Weibull distribution. Note: Beta controls the shape of the survival curve (e.g., aging or failure rate).
#' @param fertility_rates A vector of fertility rates for each age group.
#' @return A list with the following elements:
#'   \describe{
#'     \item{population}{A numeric vector giving the population size for each age group.}
#'     \item{births}{A numeric value giving the total number of births.}
#'   }
#' @export
#' @examples
#' calculate_population(0.5, 1.2, c(0.2, 0.3, 0.5, 0.4))
calculate_population <- function(alpha, beta, fertility_rates) {
  periods <- length(fertility_rates)  # The number of periods is adjusted to the length of the fertility vector
  population <- numeric(periods)      # Initialize the population vector for each period
  population[1] <- 1                  # Initial population is considered 100% (normalized value)

  # Calculate survival and population for each age
  for (i in 2:periods) {
    population[i] <- weibull_survival(alpha, beta, i-1)  # Cumulative survival to that age
  }

  # Calculate births
  births <- sum(population * fertility_rates)

  # Return both population and births
  list(population = population, births = births)
}
