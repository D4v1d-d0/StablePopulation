test_that("select_beta recovers a constrained candidate supplied as observed lx", {
  mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
  truth <- reconstruct_population(mx, beta = 1)

  selection <- select_beta(
    fertility_rates = mx,
    lx_observed = truth$lx,
    beta_values = c(0.5, 1, 1.5)
  )

  expect_s3_class(selection, "stable_population_selection")
  expect_equal(selection$best_beta, 1)
  expect_equal(selection$best_RMSE, 0, tolerance = 1e-7)
  expect_true(selection$results$selected[which(selection$results$beta == 1)])
})

test_that("derive_demographic_profile creates R, D, D_relative, and B", {
  lx <- c(1, 0.8, 0.5, 0.2)
  profile <- derive_demographic_profile(lx)

  expect_s3_class(profile, "stable_population_demographic_profile")
  expect_equal(sum(profile$R), 1, tolerance = 1e-12)
  expect_equal(sum(profile$D), profile$R[1], tolerance = 1e-12)
  expect_equal(sum(profile$D_relative), 1, tolerance = 1e-12)
  expect_equal(profile$B[1], 0.8, tolerance = 1e-12)
  expect_true(is.na(profile$B[length(profile$B)]))
})
