# StablePopulation

[![CRAN status](https://www.r-pkg.org/badges/version/StablePopulation)](https://CRAN.R-project.org/package=StablePopulation)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.r-project.org/licenses/gpl-3.0.en.html)

`StablePopulation` reconstructs discrete Weibull survivorship profiles under a
stable and stationary population assumption. Its central demographic constraint
is:

```text
sum(lx * mx) = R0 = 1
```

where `lx` is survivorship to age `x` and `mx` is age-specific fertility. Age
classes are represented internally as consecutive indices:

```text
0, 1, 2, ..., n - 1
```

## Version 1.1.0.9000: development scope

This development version extends the package while preserving the historical
Excel route documented for the CRAN release 1.0.3.

- `run_analysis()` keeps its original no-argument interface and fixed-`beta`
  Excel layout.
- `run_reconstruction_excel()` is the separate, new Excel interface for beta
  scanning, optional comparison with observed survivorship, and terminal-window
  scenarios.

The present development is linked primarily to the doctoral work of David
Palacios-Morales at the Universidad Complutense de Madrid (UCM), while retaining
his University of Burgos (UBU) affiliation. Historical CRAN releases remain part
of the earlier UBU stage and are not retroactively altered.

## Core functions retained from earlier versions

| Function | Purpose |
|---|---|
| `weibull_survival(alpha, beta, age)` | Evaluates the Weibull survivorship function. |
| `calculate_population(alpha, beta, fertility_rates)` | Returns the discrete survivorship vector and its implied births. |
| `alpha_objective(alpha, beta, fertility_rates)` | Computes `births - 1`. |
| `find_alphas(beta, fertility_rates, tol)` | Historical alpha solver. |
| `run_analysis()` | Historical Excel workflow with one beta value per worksheet. |

## Historical Excel workflow: `run_analysis()`

`run_analysis()` is retained for compatibility with the workflow described for
StablePopulation 1.0.3. It takes no arguments and reads:

```text
inst/extdata/Input_Data.xlsx
```

Each worksheet is treated as one case/species. The expected layout is:

| Location | Meaning |
|---|---|
| Column B | Fertility rates (`mx`) |
| Cell C2 | Fixed `beta` value |
| First row | Header row |

For each worksheet, the function solves for `alpha`, reconstructs the profile,
and writes a separate file named:

```text
<sheet_name>_results.xlsx
```

inside `inst/extdata/`.

## New functions in 1.1.0.9000

| Function | Purpose |
|---|---|
| `reconstruct_population()` | Reconstructs one Weibull profile for a fixed `beta` under `R0 = 1`. |
| `scan_beta()` | Generates a family of constrained profiles across candidate `beta` values. |
| `select_beta()` | Selects the constrained profile with minimum RMSE against observed `lx`. |
| `derive_demographic_profile()` | Derives stable structure `R`, exit-by-death profile `D`, relative `D`, and conditional survival `B`. |
| `fit_weibull_free()` | Fits an unconstrained two-parameter Weibull reference model. |
| `normalize_fertility()` | Rescales fertility relative to a reference `lx` so its `R0` equals 1. |
| `run_reconstruction_excel()` | New Excel interface for scan and observed-survivorship routes. |

## Two reconstruction routes

### A. Observed survivorship is available

```r
mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
lx_observed <- c(1, 0.93, 0.82, 0.67, 0.41, 0.15)

selection <- select_beta(
  fertility_rates = mx,
  lx_observed = lx_observed
)

selection$best_beta
selection$best_alpha
selection$best_RMSE
```

`select_beta()` scans candidate `beta` values, determines the corresponding
`alpha` under `R0 = 1`, and selects the smallest RMSE. It is a constrained
one-parameter selection, not a free Weibull fit.

### B. No observed survivorship is available

```r
scan <- scan_beta(
  fertility_rates = mx,
  terminal_window = c(0.0001, 0.05)
)

scan$admissible_summary
scan$terminal_extremes
```

The terminal window retains profiles whose survivorship in the last available
age class lies within the specified interval. It does not identify an empirical
optimum; it defines a range of admissible scenarios.

## From survivorship to demographic outputs

```r
profile <- derive_demographic_profile(
  lx = selection$best_lx,
  fertility_rates = mx
)

profile$table
```

This derives:

```text
lx -> R -> D -> B
```

where `R` is the normalized stable structure, `D` is the raw exit-by-death
profile, and `B` is conditional survival between consecutive classes.

## Extended Excel workflow

`run_reconstruction_excel()` is the extended interface for ordinary data
workbooks. It reads one or more sheets, preserves an age column as an output
label, and writes a separate results workbook. In an interactive R session, it
can open the operating-system file chooser directly:

```r
run_reconstruction_excel()
```

You can also provide the path explicitly. By default, the output is created
beside the input file:

```r
run_reconstruction_excel("demography.xlsx")
```

This creates:

```text
demography_StablePopulation.xlsx
```

With `mode = "auto"`, a sheet with observed survivorship uses the `select`
route; a sheet without it uses the `scan` route. Fully blank rows are ignored,
but missing or non-numeric fertility values inside a data table stop the run and
report the affected Excel rows.

Recognized headers are case-insensitive and tolerate spaces, hyphens, accents,
and underscores. Fertility aliases include:

```text
mx | m_x | fertility_rates | fertility | fecundity | fecundidad
```

Observed-survivorship aliases include:

```text
lx_observed | l_x_observed | lx | l_x | survivorship | survival | supervivencia
```

Age aliases include:

```text
age | edad | age_class | clase_edad
```

For custom headings, specify the columns explicitly:

```r
run_reconstruction_excel(
  input_file = "demography.xlsx",
  output_file = "results.xlsx",
  sheets = "Ovis_dalli",
  age_column = "Age class",
  fertility_column = "Female fertility",
  survivorship_column = "Observed survivorship"
)
```

The output workbook includes a `README` sheet, `metadata_run`, and one or more
sheets per processed input sheet:

| Pattern | Contents |
|---|---|
| `summary_<input sheet>` | Candidate beta values and numerical diagnostics. |
| `profiles_<input sheet>` | All reconstructed survivorship candidates for a scan route. |
| `selected_<input sheet>` | The selected profile, observed and reconstructed survivorship, and derived `R`, `D`, `D_relative`, and `B`. |
| `admissible_<input sheet>` | Candidates retained by an optional terminal-survivorship window. |
| `scenarios_<input sheet>` | First and last terminal-admissible profiles. |

## Installation

### CRAN release

```r
install.packages("StablePopulation")
```

### Development version

```r
install.packages("remotes")
remotes::install_github("D4v1d-d0/StablePopulation")
```

## Development checks

```r
devtools::document()
devtools::test()
devtools::check()
```

## Methodological background

The package follows the Weibull stable/stationary-population framework developed
for paleodemographic applications by Martin-Gonzalez et al. (2019). The current
development extends the reproducible R implementation toward constrained profile
selection and derived demographic quantities.

## Citation

Use:

```r
citation("StablePopulation")
```

The published CRAN release is version 1.0.3:

```text
10.32614/CRAN.package.StablePopulation
```

## License

GPL-3
