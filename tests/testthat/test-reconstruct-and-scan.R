test_that("reconstruct_population produces a constrained Weibull profile", {
  mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
  reconstruction <- reconstruct_population(mx, beta = 1)

  expect_s3_class(reconstruction, "stable_population_reconstruction")
  expect_equal(reconstruction$age, 0:5)
  expect_equal(reconstruction$lx[1], 1)
  expect_true(all(diff(reconstruction$lx) <= 0))
  expect_true(reconstruction$stable)
  expect_equal(reconstruction$R0, 1, tolerance = 1e-7)
  expect_equal(sum(reconstruction$lxmx), 1, tolerance = 1e-7)
})

test_that("scan_beta keeps all stable profiles and applies a terminal window", {
  mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
  scan <- scan_beta(mx, beta_values = c(0.5, 1, 1.5))

  expect_s3_class(scan, "stable_population_scan")
  expect_equal(nrow(scan$profiles), length(mx))
  expect_equal(nrow(scan$summary), 3)
  expect_true(all(scan$summary$stable))
  expect_true(all(scan$summary$admissible))

  terminal_scan <- scan_beta(
    mx,
    beta_values = c(0.5, 1, 1.5),
    terminal_window = c(0, 1)
  )
  expect_true(all(terminal_scan$summary$admissible))
  expect_false(is.null(terminal_scan$terminal_extremes))
  expect_equal(
    terminal_scan$terminal_extremes$first$beta,
    min(terminal_scan$summary$beta)
  )
  expect_equal(
    terminal_scan$terminal_extremes$last$beta,
    max(terminal_scan$summary$beta)
  )
})


test_that("scan_beta reports an empty terminal window explicitly", {
  mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)

  expect_warning(
    empty_scan <- scan_beta(
      mx,
      beta_values = c(0.5, 1, 1.5),
      terminal_window = c(0.0001, 0.05)
    ),
    "No stable profile satisfies the terminal window"
  )
  expect_equal(nrow(empty_scan$admissible_summary), 0)
  expect_null(empty_scan$terminal_extremes)
})
