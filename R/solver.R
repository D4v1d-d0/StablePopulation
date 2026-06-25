# Internal alpha solver used by the new StablePopulation workflow
# Solucionador interno de alpha usado por el nuevo flujo de StablePopulation

# Solve alpha under the R0 = 1 constraint without returning an endpoint as a root.
# Resolver alpha bajo la restriccion R0 = 1 sin devolver un extremo como si fuera una raiz.
# This helper is internal and intentionally not exported.
# Esta funcion auxiliar es interna y deliberadamente no se exporta.
solve_alpha <- function(
  beta,
  fertility_rates,
  tol = 1e-12,
  r0_tolerance = 1e-8,
  lower = 1e-12,
  initial_upper = 1,
  max_upper = 1e12
) {
  # Validate all scalar and vector inputs before solving.
  # Validar todos los datos escalares y vectoriales antes de resolver.
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
  lower <- validate_positive_scalar(lower, "lower")
  initial_upper <- validate_positive_scalar(initial_upper, "initial_upper")
  max_upper <- validate_positive_scalar(max_upper, "max_upper")

  if (initial_upper <= lower || max_upper <= lower) {
    stop(
      "The upper search bounds must be greater than 'lower'. / ",
      "Los limites superiores de busqueda deben ser mayores que 'lower'.",
      call. = FALSE
    )
  }

  age <- seq.int(0L, length(fertility_rates) - 1L)

  # The demographic objective is monotone non-decreasing in alpha.
  # El objetivo demografico es monotono no decreciente respecto a alpha.
  objective <- function(alpha) {
    lx <- weibull_survival(alpha = alpha, beta = beta, age = age)
    sum(lx * fertility_rates) - 1
  }

  f_lower <- objective(lower)
  if (!is.finite(f_lower)) {
    return(list(
      alpha = NA_real_, R0 = NA_real_, residual = NA_real_,
      converged = FALSE, status = "non_finite_lower_objective",
      bracket = c(lower, NA_real_), beta = beta
    ))
  }

  # A positive lower-bound objective means no interior alpha can lower R0 to 1.
  # Un objetivo positivo en el limite inferior implica que ningun alpha interior
  # puede reducir R0 hasta 1.
  if (f_lower > r0_tolerance) {
    return(list(
      alpha = NA_real_, R0 = f_lower + 1, residual = f_lower,
      converged = FALSE, status = "no_root_above_lower_bound",
      bracket = c(lower, NA_real_), beta = beta
    ))
  }

  # Retain an explicit boundary solution only when it already meets the tolerance.
  # Conservar una solucion explicita de borde solo cuando ya cumple la tolerancia.
  if (abs(f_lower) <= r0_tolerance) {
    return(list(
      alpha = lower, R0 = f_lower + 1, residual = f_lower,
      converged = TRUE, status = "lower_boundary_solution",
      bracket = c(lower, lower), beta = beta
    ))
  }

  upper <- max(initial_upper, lower * 10)
  upper <- min(upper, max_upper)
  f_upper <- objective(upper)

  # Expand the upper bound until the sign changes or the maximum bound is reached.
  # Ampliar el limite superior hasta que cambie el signo o se alcance el maximo.
  while (is.finite(f_upper) && f_upper < 0 && upper < max_upper) {
    next_upper <- min(upper * 10, max_upper)
    if (identical(next_upper, upper)) {
      break
    }
    upper <- next_upper
    f_upper <- objective(upper)
  }

  if (!is.finite(f_upper)) {
    return(list(
      alpha = NA_real_, R0 = NA_real_, residual = NA_real_,
      converged = FALSE, status = "non_finite_upper_objective",
      bracket = c(lower, upper), beta = beta
    ))
  }

  if (f_upper < 0) {
    return(list(
      alpha = NA_real_, R0 = f_upper + 1, residual = f_upper,
      converged = FALSE, status = "no_root_within_upper_bound",
      bracket = c(lower, upper), beta = beta
    ))
  }

  root <- tryCatch(
    stats::uniroot(objective, interval = c(lower, upper), tol = tol)$root,
    error = function(error) error
  )

  if (inherits(root, "error") || !is.finite(root) || root <= 0) {
    return(list(
      alpha = NA_real_, R0 = NA_real_, residual = NA_real_,
      converged = FALSE, status = "uniroot_failure",
      bracket = c(lower, upper), beta = beta
    ))
  }

  residual <- objective(root)
  R0 <- residual + 1
  converged <- is.finite(R0) && abs(residual) <= r0_tolerance

  list(
    alpha = as.numeric(root),
    R0 = as.numeric(R0),
    residual = as.numeric(residual),
    converged = converged,
    status = if (converged) "converged" else "root_outside_R0_tolerance",
    bracket = c(lower, upper),
    beta = beta
  )
}
