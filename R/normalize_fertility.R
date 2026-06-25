#' Normalize fertility using a reference survivorship profile
#'
#' Normaliza la fecundidad usando un perfil de supervivencia de referencia
#'
#' @description
#' English: Divides an age-specific fertility schedule by the net replacement
#' calculated from a reference survivorship profile. The returned schedule
#' satisfies \eqn{\sum_x l_x m_x = 1} for that reference profile.
#'
#' Espanol: Divide un calendario de fecundidad especifica por edad por el
#' reemplazo neto calculado a partir de un perfil de supervivencia de referencia.
#' El calendario devuelto satisface \eqn{\sum_x l_x m_x = 1} para ese perfil.
#'
#' @param fertility_rates Numeric vector of age-specific fertility rates. /
#'   Vector numerico de tasas de fecundidad especificas por edad.
#' @param lx_reference Numeric reference survivorship profile. It must have the
#'   same length as \code{fertility_rates}. / Perfil numerico de supervivencia
#'   de referencia. Debe tener la misma longitud que \code{fertility_rates}.
#' @param tolerance Positive numerical tolerance used to validate
#'   \code{lx_reference}. / Tolerancia numerica positiva usada para validar
#'   \code{lx_reference}.
#'
#' @return A list of class \code{"stable_population_normalized_fertility"}
#'   containing the original fertility schedule, the normalized schedule,
#'   \code{R0_reference}, the scaling factor, and a numerical check. /
#'   Una lista de clase \code{"stable_population_normalized_fertility"} con
#'   el calendario original, el calendario normalizado, \code{R0_reference},
#'   el factor de escala y una comprobacion numerica.
#'
#' @details
#' This helper prepares fertility data before they enter a reconstruction route
#' that fixes \eqn{R_0 = 1}. It does not change the internal demographic rule
#' of the package. / Esta funcion auxiliar prepara las fecundidades antes de
#' que entren en una via de reconstruccion que fija \eqn{R_0 = 1}. No cambia
#' la regla demografica interna del paquete.
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
  # Validar ambos calendarios antes de calcular el reemplazo de referencia.
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
      "Reference R0 must be positive to normalize fertility. / ",
      "El R0 de referencia debe ser positivo para normalizar la fecundidad.",
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
