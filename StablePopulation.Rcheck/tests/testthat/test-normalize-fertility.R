test_that("normalize_fertility produces R0 = 1 for the reference profile", {
  mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
  lx <- c(1, 0.90, 0.75, 0.55, 0.30, 0.10)

  normalized <- normalize_fertility(mx, lx)

  expect_s3_class(normalized, "stable_population_normalized_fertility")
  expect_equal(normalized$check_R0, 1, tolerance = 1e-12)
  expect_equal(
    sum(normalized$lx_reference * normalized$fertility_rates_normalized),
    1,
    tolerance = 1e-12
  )
})
