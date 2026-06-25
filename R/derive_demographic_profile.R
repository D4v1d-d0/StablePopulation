#' Derive stable-structure and mortality profiles from survivorship
#'
#' Deriva estructura estable y perfiles de mortalidad a partir de la supervivencia
#'
#' @description
#' English: Converts a survivorship profile \eqn{l_x} into the quantities used
#' by the demographic-to-ecological bridge in Model-V2022: the normalised stable
#' structure \eqn{R_x}, the exit-by-death profile \eqn{D_x}, its explicitly
#' relativised version, and conditional survival between consecutive classes.
#'
#' Espanol: Convierte un perfil de supervivencia \eqn{l_x} en las cantidades
#' utilizadas por el puente demografico-ecologico de Model-V2022: la estructura
#' estable normalizada \eqn{R_x}, el perfil de salida por muerte \eqn{D_x}, su
#' version relativizada explicitamente y la supervivencia condicional entre
#' clases consecutivas.
#'
#' @details
#' The stable structure is \eqn{R_x = l_x / \sum_x l_x}. The mortality profile
#' is \eqn{D_x = R_x - R_{x+1}} for all but the last class, and
#' \eqn{D_n = R_n}. Its total is \eqn{R_{x=0}} of the stable structure, not
#' necessarily one. The relativised profile is \eqn{D_x / \sum_x D_x}; it is
#' therefore a distinct quantity that sums to one.
#'
#' La estructura estable es \eqn{R_x = l_x / \sum_x l_x}. El perfil de
#' mortalidad es \eqn{D_x = R_x - R_{x+1}} para todas las clases salvo la
#' ultima, y \eqn{D_n = R_n}. Su suma es \eqn{R_{x=0}} de la estructura
#' estable, no necesariamente uno. El perfil relativizado es
#' \eqn{D_x / \sum_x D_x}; por tanto es una cantidad distinta que suma uno.
#'
#' @param lx Numeric survivorship profile. It must start at one and be
#'   non-increasing. / Perfil numerico de supervivencia. Debe comenzar en uno y
#'   ser no creciente.
#' @param fertility_rates Optional fertility schedule with the same length as
#'   \code{lx}. When supplied, the result also includes \eqn{l_xm_x} and
#'   \eqn{R_0}. / Calendario de fecundidad opcional con la misma longitud que
#'   \code{lx}. Cuando se proporciona, el resultado incluye \eqn{l_xm_x} y
#'   \eqn{R_0}.
#' @param tolerance Positive tolerance used to validate \code{lx} and assess
#'   \eqn{R_0 = 1} when fertility is provided. / Tolerancia positiva usada para
#'   validar \code{lx} y evaluar \eqn{R_0 = 1} cuando se proporciona
#'   fecundidad.
#'
#' @return A list of class \code{"stable_population_demographic_profile"}.
#'   / Una lista de clase \code{"stable_population_demographic_profile"}.
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
  # Reuse the same checks for observed and reconstructed lx vectors.
  # Reutilizar las mismas comprobaciones para vectores lx observados y reconstruidos.
  tolerance <- validate_positive_scalar(tolerance, "tolerance")
  lx <- validate_lx_observed(lx_observed = lx, tolerance = tolerance)

  age <- seq.int(0L, length(lx) - 1L)
  n_age <- length(lx)

  if (!is.null(fertility_rates)) {
    fertility_rates <- validate_fertility_rates(fertility_rates)
    if (length(fertility_rates) != n_age) {
      stop(
        "'fertility_rates' must have the same length as 'lx'. / ",
        "'fertility_rates' debe tener la misma longitud que 'lx'.",
        call. = FALSE
      )
    }
  }

  # R is the stable/stationary age structure used by the MATLAB workflow.
  # R es la estructura de edad estable/estacionaria utilizada por el flujo MATLAB.
  stable_structure <- lx / sum(lx)

  # D is the raw exit-by-death profile. Appending zero implements D_n = R_n.
  # Its total equals R at age 0, so it is not generally a probability vector.
  # D es el perfil bruto de salida por muerte. Anadir cero aplica D_n = R_n.
  # Su suma equivale a R en la edad 0, por lo que no es en general un vector de probabilidades.
  mortality_profile <- -diff(c(stable_structure, 0))

  # The relative profile is a separate, explicitly normalised quantity.
  # El perfil relativo es una cantidad separada, normalizada explicitamente.
  mortality_total <- sum(mortality_profile)
  mortality_profile_relative <- mortality_profile / mortality_total

  # B_x is survival from age x to the next consecutive class. Once lx is zero,
  # conditional survival is undefined rather than forced to 0/0.
  # B_x es la supervivencia desde la edad x hasta la siguiente clase consecutiva.
  # Cuando lx es cero, la supervivencia condicional queda indefinida y no se fuerza 0/0.
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
