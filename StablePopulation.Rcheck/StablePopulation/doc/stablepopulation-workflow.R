## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5
)

## ----setup--------------------------------------------------------------------
library(StablePopulation)

## ----input-data---------------------------------------------------------------
age <- 0:5

mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)

lx_observed <- c(
  1.0000000,
  0.8480619,
  0.7023945,
  0.5759026,
  0.4689639,
  0.3798816
)

input_data <- data.frame(
  age = age,
  mx = mx,
  lx_observed = lx_observed
)

knitr::kable(input_data, digits = 4)

## ----observed-r0--------------------------------------------------------------
R0_observed <- sum(lx_observed * mx)
R0_observed

## ----explicit-normalisation---------------------------------------------------
normalised <- normalize_fertility(
  fertility_rates = mx,
  lx_reference = lx_observed
)

normalised$R0_reference
normalised$check_R0

## ----select-beta--------------------------------------------------------------
selection <- select_beta(
  fertility_rates = mx,
  lx_observed = lx_observed,
  beta_values = seq(0.50, 1.80, by = 0.05)
)

selection

## ----selected-values----------------------------------------------------------
selection$best_beta
selection$best_alpha
selection$best_RMSE

## ----selected-profile---------------------------------------------------------
knitr::kable(selection$best_profile, digits = 4)

## ----candidate-grid-----------------------------------------------------------
selected_neighbourhood <- selection$results[
  selection$results$beta >= selection$best_beta - 0.10 &
    selection$results$beta <= selection$best_beta + 0.10,
  c("beta", "alpha", "R0", "RMSE", "selected")
]

knitr::kable(selected_neighbourhood, digits = 6)

## ----boundary-fields----------------------------------------------------------
selection$beta_at_boundary
selection$beta_boundary_note

## ----derived-profile----------------------------------------------------------
demographic_profile <- derive_demographic_profile(
  lx = selection$best_lx,
  fertility_rates = mx
)

demographic_profile

## ----derived-table------------------------------------------------------------
knitr::kable(demographic_profile$table, digits = 4)

## ----derived-checks-----------------------------------------------------------
demographic_profile$checks

## ----free-fit-----------------------------------------------------------------
free_fit <- fit_weibull_free(
  lx_observed = lx_observed,
  fertility_rates = mx
)

free_fit

## ----free-fit-profile---------------------------------------------------------
knitr::kable(free_fit$profile, digits = 4)

## ----scan-beta----------------------------------------------------------------
scan <- scan_beta(
  fertility_rates = mx,
  beta_values = seq(0.50, 1.80, by = 0.05),
  terminal_window = c(0.35, 0.39)
)

scan

## ----scan-summary-------------------------------------------------------------
knitr::kable(scan$admissible_summary, digits = 6)

## ----terminal-extremes--------------------------------------------------------
scan$terminal_extremes$first$beta
scan$terminal_extremes$last$beta

## ----fixed-beta---------------------------------------------------------------
fixed <- reconstruct_population(
  fertility_rates = mx,
  beta = 1.10
)

fixed
knitr::kable(fixed$table, digits = 4)

## ----excel-columns, echo = FALSE----------------------------------------------
excel_template <- data.frame(
  Age = age,
  mx = mx,
  lx = lx_observed,
  Beta = c(1.10, rep(NA, length(age) - 1L))
)
knitr::kable(excel_template, digits = 4)

## ----excel-call, eval = FALSE-------------------------------------------------
# run_reconstruction_excel(
#   input_file = "demography.xlsx",
#   output_file = "demography_results.xlsx",
#   mode = "auto",
#   beta_values = seq(0.05, 3.00, by = 0.05),
#   terminal_window = c(0.0001, 0.05),
#   output_detail = "standard"
# )

