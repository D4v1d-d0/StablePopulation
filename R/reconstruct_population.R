#' Reconstruct a stable Weibull survivorship profile
#'
#' Reconstruye un perfil Weibull estable de supervivencia
#'
#' @description
#' English: For a fixed Weibull shape parameter \eqn{\beta} and a fertility
#' schedule \eqn{m_x}, calculates the corresponding \eqn{\alpha} and
#' reconstructs \eqn{l_x} under the StablePopulation condition
#' \eqn{\sum_x l_xm_x = 1}.
#'
#' Espanol: Para un parametro de forma Weibull \eqn{\beta} fijo y un
#' calendario de fecundidad \eqn{m_x}, calcula el \eqn{\alpha}
#' correspondiente y reconstruye \eqn{l_x} bajo la condicion de
#' StablePopulation \eqn{\sum_x l_xm_x = 1}.
#'
#' @details
#' Age classes are generated internally as \code{0, 1, 2, ..., n - 1}.
#' The function uses the verified internal solver used by the new workflow; it
#' therefore stops rather than silently treating a search endpoint as a valid
#' root.
#'
#' Las clases de edad se generan internamente como
#' \code{0, 1, 2, ..., n - 1}. La funcion utiliza el solucionador interno
#' verificado del nuevo flujo; por tanto se detiene en vez de tratar
#' silenciosamente un extremo de busqueda como una raiz valida.
#'
#' @param fertility_rates Numeric vector of age-specific fertility rates.
#'   / Vector numerico de tasas de fecundidad especificas por edad.
#' @param beta Positive Weibull shape parameter. / Parametro de forma Weibull
#'   positivo.
#' @param tol Positive numerical tolerance for root finding. / Tolerancia
#'   numerica positiva para la busqueda de la raiz.
#' @param r0_tolerance Positive tolerance used to assess numerical agreement
#'   with \eqn{R_0 = 1}. / Tolerancia positiva utilizada para evaluar el
#'   acuerdo numerico con \eqn{R_0 = 1}.
#'
#' @return A list of class \code{"stable_population_reconstruction"} with the
#'   fitted parameters, the survivorship profile, a result table, and solver
#'   diagnostics. / Una lista de clase \code{"stable_population_reconstruction"}
#'   con los parametros ajustados, el perfil de supervivencia, una tabla de
#'   resultados y diagnosticos del solucionador.
#'
#' @examples
#' mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
#' reconstruction <- reconstruct_population(mx, beta = 1.10)
#' reconstruction$alpha
#' reconstruction$table
#'
#' @export
reconstruct_population <- function(
  fertility_rates,
  beta,
  tol = 1e-12,
  r0_tolerance = 1e-8
) {
  # Validate all inputs once.
  # Validar todos los datos una sola vez.
  fertility_rates <- validate_fertility_rates(fertility_rates)
  beta <- validate_beta_values(beta)
  if (length(beta) != 1L) {
    stop(
      "'beta' must contain exactly one positive value. / ",
      "'beta' debe contener exactamente un valor positivo.",
      call. = FALSE
    )
  }
  tol <- validate_positive_scalar(tol, "tol")
  r0_tolerance <- validate_positive_scalar(r0_tolerance, "r0_tolerance")

  # StablePopulation uses consecutive class indices, not an external age scale.
  # StablePopulation usa indices de clase consecutivos, no una escala de edad externa.
  age <- seq.int(0L, length(fertility_rates) - 1L)

  # Solve alpha under the R0 = 1 restriction.
  # Resolver alpha bajo la restriccion R0 = 1.
  solver <- solve_alpha(
    beta = beta,
    fertility_rates = fertility_rates,
    tol = tol,
    r0_tolerance = r0_tolerance
  )

  if (!isTRUE(solver$converged)) {
    stop(
      "No valid alpha was found under the R0 = 1 constraint (status: ",
      solver$status, "). / No se encontro un alpha valido bajo la restriccion ",
      "R0 = 1 (estado: ", solver$status, ").",
      call. = FALSE
    )
  }

  alpha <- solver$alpha
  lx <- weibull_survival(alpha = alpha, beta = beta, age = age)
  lx[1L] <- 1
  lxmx <- lx * fertility_rates
  R0 <- sum(lxmx)
  residual <- R0 - 1
  stable <- abs(residual) <= r0_tolerance

  # An additional guard protects against unexpected numerical inconsistencies.
  # Una comprobacion adicional protege frente a incoherencias numericas inesperadas.
  if (!stable) {
    stop(
      "The reconstructed profile did not satisfy the requested R0 tolerance. / ",
      "El perfil reconstruido no cumplio la tolerancia R0 solicitada.",
      call. = FALSE
    )
  }

  output_table <- data.frame(
    age = age,
    fertility_rates = fertility_rates,
    lx = lx,
    lxmx = lxmx,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      model = "weibull_R0_1",
      constrained_R0 = TRUE,
      age = age,
      fertility_rates = fertility_rates,
      beta = beta,
      alpha = alpha,
      lx = lx,
      population = lx,
      lxmx = lxmx,
      R0 = R0,
      births = R0,
      residual = residual,
      stable = stable,
      solver = solver,
      tolerance = list(root = tol, R0 = r0_tolerance),
      table = output_table
    ),
    class = "stable_population_reconstruction"
  )
}
