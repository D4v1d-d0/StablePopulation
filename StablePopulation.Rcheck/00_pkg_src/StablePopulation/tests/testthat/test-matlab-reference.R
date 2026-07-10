test_that("C. fiber reproduces the saved MATLAB demographic reference", {
  reference <- matlab_reference_cases$c_fiber_beta_050

  reconstruction <- reconstruct_population(
    fertility_rates = reference$mx,
    beta = reference$beta
  )
  profile <- derive_demographic_profile(
    lx = reconstruction$lx,
    fertility_rates = reference$mx
  )

  # MATLAB lambda is the same Weibull scale parameter as R alpha.
  # El lambda de MATLAB es el mismo parámetro de escala Weibull que alpha en R.
  expect_equal(
    reconstruction$alpha,
    reference$alpha,
    tolerance = matlab_reference_tolerance
  )
  expect_equal(
    reconstruction$lx,
    reference$lx,
    tolerance = matlab_reference_tolerance
  )
  expect_equal(profile$R, reference$R, tolerance = matlab_reference_tolerance)
  expect_equal(profile$D, reference$D, tolerance = matlab_reference_tolerance)
  expect_equal(
    profile$B[-length(profile$B)],
    reference$B,
    tolerance = matlab_reference_tolerance
  )
  expect_true(is.na(profile$B[length(profile$B)]))
  expect_equal(reconstruction$R0, 1, tolerance = matlab_reference_tolerance)
})

test_that("C. elaphus reproduces the saved MATLAB demographic reference", {
  reference <- matlab_reference_cases$c_elaphus_beta_050

  reconstruction <- reconstruct_population(
    fertility_rates = reference$mx,
    beta = reference$beta
  )
  profile <- derive_demographic_profile(
    lx = reconstruction$lx,
    fertility_rates = reference$mx
  )

  expect_equal(
    reconstruction$alpha,
    reference$alpha,
    tolerance = matlab_reference_tolerance
  )
  expect_equal(
    reconstruction$lx,
    reference$lx,
    tolerance = matlab_reference_tolerance
  )
  expect_equal(profile$R, reference$R, tolerance = matlab_reference_tolerance)
  expect_equal(profile$D, reference$D, tolerance = matlab_reference_tolerance)
  expect_equal(
    profile$B[-length(profile$B)],
    reference$B,
    tolerance = matlab_reference_tolerance
  )
  expect_true(is.na(profile$B[length(profile$B)]))
  expect_equal(reconstruction$R0, 1, tolerance = matlab_reference_tolerance)
})

test_that("C. fiber reproduces the MATLAB terminal-window scenario range", {
  c_fiber <- matlab_reference_cases$c_fiber_beta_050
  terminal_reference <- matlab_reference_cases$c_fiber_terminal_window

  scan <- scan_beta(
    fertility_rates = c_fiber$mx,
    beta_values = terminal_reference$beta_values,
    terminal_window = terminal_reference$window
  )

  expect_equal(
    scan$admissible_summary$beta,
    terminal_reference$admissible_beta,
    tolerance = matlab_reference_tolerance
  )
  expect_equal(
    scan$terminal_extremes$first$beta,
    terminal_reference$first$beta,
    tolerance = matlab_reference_tolerance
  )
  expect_equal(
    scan$terminal_extremes$first$alpha,
    terminal_reference$first$alpha,
    tolerance = matlab_reference_tolerance
  )
  expect_equal(
    scan$terminal_extremes$first$lx_terminal,
    terminal_reference$first$lx_terminal,
    tolerance = matlab_reference_tolerance
  )
  expect_equal(
    scan$terminal_extremes$last$beta,
    terminal_reference$last$beta,
    tolerance = matlab_reference_tolerance
  )
  expect_equal(
    scan$terminal_extremes$last$alpha,
    terminal_reference$last$alpha,
    tolerance = matlab_reference_tolerance
  )
  expect_equal(
    scan$terminal_extremes$last$lx_terminal,
    terminal_reference$last$lx_terminal,
    tolerance = matlab_reference_tolerance
  )
})
