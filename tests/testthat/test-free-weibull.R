test_that("fit_weibull_free produces a finite unconstrained reference fit", {
  age <- 0:7
  lx <- weibull_survival(alpha = 4, beta = 1.2, age = age)
  lx[1] <- 1

  fit <- fit_weibull_free(
    lx_observed = lx,
    start_alpha = 4,
    start_beta = 1.2
  )

  expect_s3_class(fit, "stable_population_free_fit")
  expect_true(is.finite(fit$alpha))
  expect_true(is.finite(fit$beta))
  expect_lt(fit$RMSE, 1e-5)
})
