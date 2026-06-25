# Validation helpers for StablePopulation
# Funciones auxiliares de validacion para StablePopulation

#' Validate age-specific fertility rates
#'
#' Valida tasas de fecundidad especificas por edad
#'
#' @description
#' English: Checks that an age-specific fertility schedule is a finite,
#' one-dimensional, non-negative numeric vector with at least one positive
#' value. StablePopulation interprets positions as consecutive age classes
#' 0, 1, 2, ... .
#'
#' Espanol: Comprueba que un calendario de fecundidad especifica por edad sea
#' un vector numerico, unidimensional, finito y no negativo, con al menos un
#' valor positivo. StablePopulation interpreta sus posiciones como clases de
#' edad consecutivas 0, 1, 2, ... .
#'
#' @param fertility_rates Numeric vector of non-negative age-specific fertility
#'   rates. / Vector numerico de tasas de fecundidad especificas por edad no
#'   negativas.
#' @param min_length Minimum permitted number of age classes. / Numero minimo
#'   permitido de clases de edad.
#'
#' @return A validated numeric vector. / Un vector numerico validado.
#'
#' @keywords internal
validate_fertility_rates <- function(fertility_rates, min_length = 2L) {
  # Validate the requested minimum length.
  # Validar la longitud minima solicitada.
  if (!is.numeric(min_length) || length(min_length) != 1L ||
      !is.finite(min_length) || min_length < 1 ||
      min_length != as.integer(min_length)) {
    stop(
      "'min_length' must be one positive whole number. / ",
      "'min_length' debe ser un unico numero entero positivo.",
      call. = FALSE
    )
  }
  min_length <- as.integer(min_length)

  # Require a plain numeric vector, not a matrix or data frame.
  # Exigir un vector numerico simple, no una matriz ni un data frame.
  if (!is.numeric(fertility_rates) || !is.atomic(fertility_rates) ||
      !is.null(dim(fertility_rates))) {
    stop(
      "'fertility_rates' must be a numeric vector. / ",
      "'fertility_rates' debe ser un vector numerico.",
      call. = FALSE
    )
  }

  if (length(fertility_rates) < min_length) {
    stop(
      "'fertility_rates' must contain at least ", min_length,
      " age classes. / 'fertility_rates' debe contener al menos ",
      min_length, " clases de edad.",
      call. = FALSE
    )
  }

  # Reject NA, NaN, Inf, and -Inf.
  # Rechazar NA, NaN, Inf e -Inf.
  if (any(!is.finite(fertility_rates))) {
    stop(
      "'fertility_rates' cannot contain NA, NaN, or infinite values. / ",
      "'fertility_rates' no puede contener valores NA, NaN o infinitos.",
      call. = FALSE
    )
  }

  if (any(fertility_rates < 0)) {
    stop(
      "'fertility_rates' cannot contain negative values. / ",
      "'fertility_rates' no puede contener valores negativos.",
      call. = FALSE
    )
  }

  if (!any(fertility_rates > 0)) {
    stop(
      "'fertility_rates' must contain at least one positive value. / ",
      "'fertility_rates' debe contener al menos un valor positivo.",
      call. = FALSE
    )
  }

  # Coerce consistently while retaining supplied names.
  # Convertir de forma consistente conservando los nombres suministrados.
  supplied_names <- names(fertility_rates)
  fertility_rates <- as.numeric(fertility_rates)
  if (!is.null(supplied_names)) {
    names(fertility_rates) <- supplied_names
  }

  fertility_rates
}

#' Validate an observed or reconstructed survivorship profile
#'
#' Valida un perfil observado o reconstruido de supervivencia
#'
#' @description
#' English: Checks that a survivorship vector \eqn{l_x} is finite,
#' one-dimensional, bounded between zero and one, starts at one, and is
#' non-increasing with age. Tiny rounding deviations within \code{tolerance}
#' are accepted and then clipped to the interval \code{[0, 1]}.
#'
#' Espanol: Comprueba que un vector de supervivencia \eqn{l_x} sea finito,
#' unidimensional, este acotado entre cero y uno, comience en uno y sea no
#' creciente con la edad. Se aceptan pequenas desviaciones de redondeo dentro
#' de \code{tolerance}, que despues se recortan al intervalo \code{[0, 1]}.
#'
#' @param lx_observed Numeric vector of survivorship values. / Vector numerico
#'   de valores de supervivencia.
#' @param expected_length Optional expected length. / Longitud esperada
#'   opcional.
#' @param tolerance Positive numeric tolerance for validation. / Tolerancia
#'   numerica positiva para la validacion.
#' @param min_length Minimum permitted number of age classes. / Numero minimo
#'   permitido de clases de edad.
#'
#' @return A validated numeric survivorship vector. / Un vector numerico de
#'   supervivencia validado.
#'
#' @keywords internal
validate_lx_observed <- function(
  lx_observed,
  expected_length = NULL,
  tolerance = 1e-8,
  min_length = 2L
) {
  # Validate tolerance and requested lengths before using them.
  # Validar tolerancia y longitudes solicitadas antes de utilizarlas.
  tolerance <- validate_positive_scalar(tolerance, "tolerance")

  if (!is.numeric(min_length) || length(min_length) != 1L ||
      !is.finite(min_length) || min_length < 1 ||
      min_length != as.integer(min_length)) {
    stop(
      "'min_length' must be one positive whole number. / ",
      "'min_length' debe ser un unico numero entero positivo.",
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
        min_length, ". / 'expected_length' debe ser NULL o un unico numero ",
        "entero no menor que ", min_length, ".",
        call. = FALSE
      )
    }
    expected_length <- as.integer(expected_length)
  }

  # Require a plain numeric vector, not a matrix or data frame.
  # Exigir un vector numerico simple, no una matriz ni un data frame.
  if (!is.numeric(lx_observed) || !is.atomic(lx_observed) ||
      !is.null(dim(lx_observed))) {
    stop(
      "'lx_observed' must be a numeric vector. / ",
      "'lx_observed' debe ser un vector numerico.",
      call. = FALSE
    )
  }

  if (length(lx_observed) < min_length) {
    stop(
      "'lx_observed' must contain at least ", min_length,
      " age classes. / 'lx_observed' debe contener al menos ",
      min_length, " clases de edad.",
      call. = FALSE
    )
  }

  if (!is.null(expected_length) && length(lx_observed) != expected_length) {
    stop(
      "'lx_observed' must have length ", expected_length,
      ". / 'lx_observed' debe tener longitud ", expected_length, ".",
      call. = FALSE
    )
  }

  if (any(!is.finite(lx_observed))) {
    stop(
      "'lx_observed' cannot contain NA, NaN, or infinite values. / ",
      "'lx_observed' no puede contener valores NA, NaN o infinitos.",
      call. = FALSE
    )
  }

  if (any(lx_observed < -tolerance) || any(lx_observed > 1 + tolerance)) {
    stop(
      "All values in 'lx_observed' must be between 0 and 1. / ",
      "Todos los valores de 'lx_observed' deben estar entre 0 y 1.",
      call. = FALSE
    )
  }

  if (abs(lx_observed[1L] - 1) > tolerance) {
    stop(
      "The first value of 'lx_observed' must be 1. / ",
      "El primer valor de 'lx_observed' debe ser 1.",
      call. = FALSE
    )
  }

  # Survivorship can stay constant or decrease, but cannot increase.
  # La supervivencia puede mantenerse o disminuir, pero no aumentar.
  if (any(diff(lx_observed) > tolerance)) {
    stop(
      "'lx_observed' must be non-increasing with age. / ",
      "'lx_observed' debe ser no creciente con la edad.",
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
# Validar un vector de valores beta Weibull.
# This helper is internal and intentionally not exported.
# Esta funcion auxiliar es interna y deliberadamente no se exporta.
validate_beta_values <- function(beta_values) {
  if (!is.numeric(beta_values) || !is.atomic(beta_values) ||
      !is.null(dim(beta_values)) || length(beta_values) == 0L ||
      any(!is.finite(beta_values))) {
    stop(
      "'beta_values' must be a non-empty numeric vector of finite values. / ",
      "'beta_values' debe ser un vector numerico no vacio de valores finitos.",
      call. = FALSE
    )
  }

  if (any(beta_values <= 0)) {
    stop(
      "All values in 'beta_values' must be strictly positive. / ",
      "Todos los valores de 'beta_values' deben ser estrictamente positivos.",
      call. = FALSE
    )
  }

  sort(unique(as.numeric(beta_values)))
}

# Validate a positive scalar.
# Validar un escalar positivo.
# This helper is internal and intentionally not exported.
# Esta funcion auxiliar es interna y deliberadamente no se exporta.
validate_positive_scalar <- function(value, argument_name) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      value <= 0) {
    stop(
      "'", argument_name, "' must be one positive finite numeric value. / '",
      argument_name,
      "' debe ser un unico valor numerico positivo y finito.",
      call. = FALSE
    )
  }

  as.numeric(value)
}

# Validate an optional terminal survivorship window.
# Validar una ventana opcional de supervivencia terminal.
# This helper is internal and intentionally not exported.
# Esta funcion auxiliar es interna y deliberadamente no se exporta.
validate_terminal_window <- function(terminal_window) {
  if (is.null(terminal_window)) {
    return(NULL)
  }

  if (!is.numeric(terminal_window) || length(terminal_window) != 2L ||
      any(!is.finite(terminal_window)) || terminal_window[1L] < 0 ||
      terminal_window[2L] > 1 || terminal_window[1L] > terminal_window[2L]) {
    stop(
      "'terminal_window' must be NULL or two finite values in [0, 1] in ",
      "non-decreasing order. / 'terminal_window' debe ser NULL o dos valores ",
      "finitos en [0, 1] en orden no decreciente.",
      call. = FALSE
    )
  }

  as.numeric(terminal_window)
}

# Validate lower and upper optimisation bounds.
# Validar los limites inferior y superior de una optimizacion.
# This helper is internal and intentionally not exported.
# Esta funcion auxiliar es interna y deliberadamente no se exporta.
validate_positive_bounds <- function(bounds, argument_name) {
  if (!is.numeric(bounds) || length(bounds) != 2L ||
      any(!is.finite(bounds)) || any(bounds <= 0) || bounds[1L] >= bounds[2L]) {
    stop(
      "'", argument_name, "' must contain two finite positive values with ",
      "lower < upper. / '", argument_name,
      "' debe contener dos valores positivos y finitos con inferior < superior.",
      call. = FALSE
    )
  }

  as.numeric(bounds)
}
