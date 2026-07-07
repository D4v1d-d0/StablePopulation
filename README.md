# StablePopulation

[![CRAN status](https://www.r-pkg.org/badges/version/StablePopulation)](https://CRAN.R-project.org/package=StablePopulation)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.r-project.org/licenses/gpl-3.0.en.html)

`StablePopulation` reconstructs discrete Weibull survivorship profiles under a
stable and stationary population assumption. The package is the demographic
layer of a broader modelling workflow: it reconstructs a species-level profile
and derives the quantities needed later to connect demography with biomass and
community models.

Its central constraint is:

```text
sum(lx * mx) = R0 = 1
```

where `lx` is survivorship to class `x` and `mx` is age-specific fertility.
Calculations always use consecutive internal class indices:

```text
0, 1, 2, ..., n - 1
```

An age column supplied in Excel is retained as an output label, but it does not
change this internal index. The package therefore assumes consecutive classes;
it does not treat an Excel age label as a separate numerical time scale.

## Development status

The CRAN release is version 1.0.3. The GitHub development version,
1.1.0.9000, preserves the historical workflow and adds explicit reconstruction
routes, robust numerical diagnostics, and the demographic quantities needed by
later ecological modules.

The development is linked primarily to the doctoral work of David
Palacios-Morales at the Universidad Complutense de Madrid (UCM), while retaining
his University of Burgos (UBU) affiliation.

## Choose a reconstruction route

The most useful first question is: **what information do you have?**

| Available information | Function | What the result means |
|---|---|---|
| A chosen Weibull shape `beta` | `reconstruct_population()` | One constrained profile under `R0 = 1`. |
| Fertility `mx` and observed survivorship `lx` | `select_beta()` | The constrained candidate with the smallest RMSE against observed `lx`. |
| Fertility `mx`, but no observed `lx` | `scan_beta()` | A family of constrained candidate scenarios; optionally filtered by terminal survivorship. |
| Observed `lx` only, or a descriptive comparison | `fit_weibull_free()` | A free two-parameter Weibull fit; it does **not** impose `R0 = 1`. |

`select_beta()` and `scan_beta()` solve a different scientific problem. The
first selects a profile using observed evidence. The second retains a set of
plausible scenarios when such evidence is unavailable. Neither route should be
presented as the other.

## Route A: observed survivorship is available

Use `select_beta()` when a fertility schedule and an observed survivorship
profile have the same number of consecutive classes.

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
selection$best_profile
```

For every candidate `beta`, the package solves the corresponding `alpha` under
`R0 = 1`, reconstructs `lx`, and calculates RMSE against the observed profile.
`select_beta()` then returns the smallest-RMSE candidate. This is a
**constrained one-parameter selection**, not a free two-parameter Weibull fit.

### Explicit fertility normalisation

Observed fertility and survivorship may imply a reference net replacement value
other than one:

```r
R0_observed <- sum(lx_observed * mx)
```

When the analytical decision is to rescale that observed schedule to the stable
and stationary condition while preserving its age pattern, do so explicitly:

```r
normalised <- normalize_fertility(
  fertility_rates = mx,
  lx_reference = lx_observed
)

selection <- select_beta(
  fertility_rates = normalised$fertility_rates_normalized,
  lx_observed = lx_observed
)
```

`normalize_fertility()` multiplies every fertility value by the same factor; it
changes the overall scale, not the shape of the schedule. `select_beta()` and
`run_reconstruction_excel()` never normalise fertility silently. This keeps the
choice visible: raw schedules and schedules deliberately rescaled to `R0 = 1`
answer different methodological questions.

## Route B: no observed survivorship is available

Use `scan_beta()` to generate candidate profiles. A terminal-survivorship window
can retain scenarios for which the last available class has a small but non-zero
survivorship value.

```r
scan <- scan_beta(
  fertility_rates = mx,
  beta_values = seq(0.05, 3.00, by = 0.05),
  terminal_window = c(0.0001, 0.05)
)

scan$admissible_summary
scan$terminal_extremes
```

The terminal window applies the inclusive condition:

```text
lower <= lx at the final class <= upper
```

It does **not** estimate a true `beta`, select an empirical optimum, or create a
central profile. It defines a set of scenarios compatible with the `R0 = 1`
constraint and the stated terminal criterion.

When a terminal window is active, `terminal_extremes$first` and
`terminal_extremes$last` are the first and last admissible profiles in ascending
`beta` order. This preserves the historical Model-V2022 convention. They are
scenario endpoints, not automatically the smallest and largest values of every
downstream ecological quantity.

## Route C: a fixed beta is already justified

```r
reconstruction <- reconstruct_population(
  fertility_rates = mx,
  beta = 1.10
)

reconstruction$alpha
reconstruction$lx
reconstruction$table
```

This route is appropriate when `beta` has been selected elsewhere or is being
examined as a specific hypothesis. It calculates `alpha` under `R0 = 1` and
returns solver diagnostics as well as the profile.

## Free Weibull reference fit

`fit_weibull_free()` is available for descriptive comparison with an observed
survivorship profile:

```r
free_fit <- fit_weibull_free(
  lx_observed = lx_observed,
  fertility_rates = mx
)

free_fit$alpha
free_fit$beta
free_fit$RMSE
free_fit$R0_fitted
```

Both Weibull parameters are free in this fit, and `R0_fitted` is reported only
afterwards when fertility is supplied. It is therefore not interchangeable with
`select_beta()` and is not the default StablePopulation reconstruction route.

## From survivorship to demographic outputs

A reconstructed profile can be translated into the demographic quantities used
by the subsequent species and ecological layers:

```r
profile <- derive_demographic_profile(
  lx = selection$best_lx,
  fertility_rates = mx
)

profile$table
```

The derived quantities are:

```text
R_x          = lx_x / sum(lx)
D_x          = R_x - R_(x+1), with D_last = R_last
D_relative_x = D_x / sum(D)
B_x          = lx_(x+1) / lx_x
```

- `R` is the normalized stable or stationary age structure and sums to one.
- `D` is the raw exit-by-death profile derived from `R`. Its total equals the
  first value of `R`, so it is not generally a probability vector.
- `D_relative` is the separately normalised version of `D` and sums to one.
- `B` is conditional survival from one consecutive class to the next; it is
  `NA` where the ratio is undefined.

This completes the demographic bridge:

```text
mx -> lx -> R -> D -> B
```

The next scientific layer, outside the present package scope, can combine `D`
with body mass by class and species density to calculate mortality biomass.

## Extended Excel workflow

`run_reconstruction_excel()` is the recommended interface for ordinary Excel
workbooks. In an interactive R session, the simplest use opens the operating-
system file chooser:

```r
run_reconstruction_excel()
```

You can also provide a workbook path explicitly:

```r
run_reconstruction_excel("demography.xlsx")
```

By default this creates:

```text
demography_StablePopulation.xlsx
```

### Automatic route selection

With `mode = "auto"`, each input sheet follows one of these routes:

| Available input | Route | Interpretation |
|---|---|---|
| Observed survivorship (`lx`) | `select` | Scans beta values and selects the constrained profile with the lowest RMSE. |
| No `lx`, one positive `Beta` value | `fixed` | Reconstructs one profile using the supplied beta. |
| Neither `lx` nor `Beta` | `scan` | Produces candidate scenarios; it never selects an arbitrary profile. |

Recognised headers are case-insensitive and tolerate spaces, hyphens, accents,
underscores, units, and descriptive text. Examples include `Age (years)`,
`mx (Fertility Rate)`, `lx (Survivorship)`, and `Beta`.

For custom headings, specify the columns explicitly:

```r
run_reconstruction_excel(
  input_file = "demography.xlsx",
  output_file = "results.xlsx",
  sheets = "Ovis_dalli",
  age_column = "Age class",
  fertility_column = "Female fertility",
  survivorship_column = "Observed survivorship",
  beta_column = "Beta"
)
```

The Excel workflow analyses fertility exactly as supplied. When explicit
normalisation is required, prepare the schedule first in R with
`normalize_fertility()` and save the resulting values to the workbook.

### Output workbooks

The default `output_detail = "standard"` workbook contains:

```text
Overview
Result_<input sheet 1>
Result_<input sheet 2>
...
```

`Overview` records the route, selected parameters where applicable, RMSE for a
selection route, numerical `R0` checks, scan status, and notes about skipped or
non-unique cases. Selected and fixed-beta result sheets contain age labels,
fertility, observed survivorship when available, reconstructed survivorship,
`R`, `D`, `D_relative`, and `B`.

A scan result never promotes the first beta to a result merely for convenience.
It lists stable candidates, or terminal-window-admissible candidates when a
window is supplied, and states why no unique profile is shown.

For audit or method development, request the complete workbook:

```r
run_reconstruction_excel(
  "demography.xlsx",
  output_detail = "full"
)
```

The full workbook also contains candidate tables, scan profiles, and final
metadata.

## Legacy source-tree workflow: `run_analysis()`

`run_analysis()` is retained for compatibility with the StablePopulation 1.0.3
workflow. It has no arguments and reads:

```text
inst/extdata/Input_Data.xlsx
```

Each worksheet uses fertility in column B, one fixed `beta` value in cell C2,
and a header row. The function writes `<sheet_name>_results.xlsx` inside the
source-tree `inst/extdata/` directory.

> **Important:** `run_analysis()` is supported only from a Git checkout or an
> unpacked source archive that retains `inst/extdata/Input_Data.xlsx`. The
> historical workbook is excluded from built package artefacts and installed
> package libraries are generally not writable. Do not use `run_analysis()` as
> the interface for a package installed from CRAN or GitHub.

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

## Methodological background and citation

The package follows the Weibull stable/stationary-population framework developed
for paleodemographic applications by Martín-González, Rodríguez-Gómez and
Palmqvist (2019).

Use:

```r
citation("StablePopulation")
```

The published CRAN release is version 1.0.3 and has the package DOI:

```text
10.32614/CRAN.package.StablePopulation
```

The development version should be cited as the GitHub development version shown
by `citation("StablePopulation")`.

## License

GPL-3
