# StablePopulation

[![CRAN status](https://www.r-pkg.org/badges/version/StablePopulation)](https://CRAN.R-project.org/package=StablePopulation)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.r-project.org/licenses/gpl-3.0.en.html)

`StablePopulation` is an R package for reconstructing **age-specific survivorship profiles** under a **stable and stationary population** assumption using a **Weibull survival model**.

For a chosen value of **beta** and a vector of **age-specific fertility rates**, the package computes the corresponding **alpha** value that satisfies the demographic constraint:

```text
sum(lx * mx) = 1
```

where the survivorship profile is defined by the Weibull model:

```text
lx = exp(- (x / alpha)^beta )
```

In other words, the package helps you go from a fertility schedule and a fixed `beta` to a consistent survivorship profile.

---

## What the package does

`StablePopulation` provides the core tools needed to:

- evaluate Weibull survival at a given age,
- calculate the survivorship / normalized population profile by age,
- solve for `alpha` from a fertility schedule and a chosen `beta`,
- process multiple cases from an Excel file and export results automatically.

This makes it useful for demographic and paleoecological workflows in which a stable and stationary population profile must be reconstructed from fertility data.

---

## Installation

### Install from CRAN

```r
install.packages("StablePopulation")
```

### Install the development version from GitHub

```r
install.packages("remotes")
remotes::install_github("D4v1d-d0/StablePopulation")
```

---

## Quick start

```r
library(StablePopulation)

fertility_rates <- c(0, 0, 0.315, 0.400, 0.895, 1.244, 1.440,
                     1.581, 1.545, 1.365, 1.131, 0.953,
                     0.622, 0.437, 0.368)

beta <- 0.5

# Solve for alpha
alpha <- find_alphas(beta, fertility_rates)
alpha

# Compute the corresponding profile
result <- calculate_population(alpha, beta, fertility_rates)

# Survivorship / normalized population profile
result$population

# Total births implied by the profile
result$births
```

A typical workflow is:

1. choose a value of `beta`,
2. estimate `alpha` with `find_alphas()`,
3. compute the age profile with `calculate_population()`,
4. verify that births are effectively equal to 1.

---

## Main functions

| Function | Purpose |
|---|---|
| `weibull_survival(alpha, beta, age)` | Computes survival to a given age under the Weibull model. |
| `calculate_population(alpha, beta, fertility_rates)` | Returns the age-specific population profile and the total number of births. |
| `alpha_objective(alpha, beta, fertility_rates)` | Objective function used for root finding (`births - 1`). |
| `find_alphas(beta, fertility_rates, tol = 1e-22)` | Solves for `alpha` given `beta` and fertility rates. |
| `run_analysis()` | Reads an Excel file, processes all sheets, and writes one output workbook per case/species. |

---

## Excel workflow

The package includes a simple Excel-based workflow through `run_analysis()`.

### Expected input file

The function looks for this file inside the package project structure:

```text
inst/extdata/Input_Data.xlsx
```

Each worksheet is treated as one case/species.

### Expected worksheet layout

In the current implementation, `run_analysis()` expects:

| Location | Meaning |
|---|---|
| Column B | Fertility rates (`mx`) |
| Cell C2 | `beta` value |
| First row | Header row |

### What `run_analysis()` does

For each sheet in `Input_Data.xlsx`, the function:

1. reads the fertility schedule,
2. reads the corresponding `beta`,
3. solves for `alpha`,
4. computes the population profile,
5. writes an output file named:

```text
<sheet_name>_results.xlsx
```

in:

```text
inst/extdata/
```

### Important note

`run_analysis()` is designed around the current source-project structure of the repository, where the package root folder is named `StablePopulation`.

---

## Returned objects

### `calculate_population()`
Returns a list with two elements:

- `population`: numeric vector with the population size / survivorship profile by age,
- `births`: numeric value with the total births implied by that profile.

### `find_alphas()`
Returns a single numeric value: the estimated `alpha` associated with the supplied `beta` and fertility schedule.

---

## Documentation

Official CRAN documentation:

- CRAN page: <https://CRAN.R-project.org/package=StablePopulation>
- Reference manual (HTML): <https://cran.r-project.org/web/packages/StablePopulation/refman/StablePopulation.html>
- Reference manual (PDF): <https://cran.r-project.org/web/packages/StablePopulation/StablePopulation.pdf>

---

## Methodological background

The package description states that its methods are inspired by:

> Martín-González et al. (2019). *Survival profiles from linear models versus Weibull models: Estimating stable and stationary population structures for Pleistocene large mammals*.

---

## Citation

To obtain the package citation from R:

```r
citation("StablePopulation")
```

CRAN DOI:

```text
10.32614/CRAN.package.StablePopulation
```

---

## Authors

- **David Palacios-Morales**
- **Guillermo Rodríguez-Gómez**
- **Jesús A. Martín-González**

Maintainer: **David Palacios-Morales** (<dpmorales@ubu.es>)

---

## License

GPL-3
