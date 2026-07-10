test_that("internal validators accept coherent demographic vectors", {
  mx <- StablePopulation:::validate_fertility_rates(c(0, 0, 0.2, 0.5))
  lx <- StablePopulation:::validate_lx_observed(c(1, 0.9, 0.6, 0.2))

  expect_equal(mx, c(0, 0, 0.2, 0.5))
  expect_equal(lx, c(1, 0.9, 0.6, 0.2))
})

test_that("internal validators reject incoherent demographic vectors", {
  expect_error(
    StablePopulation:::validate_fertility_rates(c(0, -0.1, 0.2)),
    "negative"
  )
  expect_error(
    StablePopulation:::validate_fertility_rates(c(0, 0, 0)),
    "positive"
  )
  expect_error(
    StablePopulation:::validate_lx_observed(c(1, 0.7, 0.8)),
    "non-increasing"
  )
  expect_error(
    StablePopulation:::validate_lx_observed(c(0.9, 0.7, 0.4)),
    "must be 1"
  )
})
