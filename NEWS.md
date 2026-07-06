# NEWS for StablePopulation

## 1.1.0.9000 - 2026-06-25

Development version prepared during the UCM doctoral stage.

- Clarifies `run_analysis()` as a legacy source-tree workflow. Its historical
  interface and behavior are unchanged; `run_reconstruction_excel()` remains
  the recommended interface for installed-package and new-analysis use.

- Extends `run_reconstruction_excel()` to recognize descriptive legacy headings
  such as `Age (years)`, `mx (Fertility Rate)`, and `lx (Survivorship)`.
  When no observed survivorship is present, `mode = "auto"` now uses a
  recognized one-value `Beta` column for a fixed-beta reconstruction before
  falling back to a beta scan.

- Standardizes source comments, user-facing messages, and package documentation in English.

- Restores and preserves `run_analysis()` exactly as the historical
  StablePopulation 1.0.3 Excel workflow: no function arguments, one fixed
  beta value in cell C2 of each worksheet, fertility rates in column B, and one
  output workbook per worksheet/species.
- Adds `run_reconstruction_excel()` as a separate extended Excel workflow for
  beta scanning, observed-survivorship selection, and terminal-window scenarios.
  When called without `input_file` in an interactive R session, it opens the
  operating-system file chooser.
- Adds `reconstruct_population()` for one constrained Weibull reconstruction
  under `R0 = 1`.
- Adds `scan_beta()` to generate and diagnose candidate Weibull profiles under
  `R0 = 1`, optionally retaining profiles in a terminal-survivorship window.
- Adds `select_beta()` to select the constrained candidate with the lowest RMSE
  against observed survivorship.
- Adds `derive_demographic_profile()` to derive the stable structure `R`, the
  exit-by-death profile `D`, the explicitly relative death profile, and
  conditional survival `B`.
- Adds `fit_weibull_free()` as a descriptive free two-parameter Weibull
  reference fit.
- Adds `normalize_fertility()` to prepare a fertility schedule relative to a
  reference survivorship profile before a reconstruction fixed at `R0 = 1`.
- Adds shared input validation, a robust internal alpha solver, test coverage,
  and UCM-first author-affiliation metadata for the present development stage.
- Does not yet add the inverse alpha-to-beta exploration route (`solve_beta()`
  and `scan_alpha()`); it remains available in historical development material
  until a concrete analytical use case justifies exposing it here.

## 1.0.3 - 2025-07-24

- Added reference to a relevant scientific article in the Description field.
- Improved the documentation for all exported functions: included the
  structure, type, and meaning of the results in the `@return` section.
- Updated `run_analysis()` to use `message()` for console output (instead of
  `cat()`), improving R compatibility and meeting CRAN requirements.
- Replaced Unicode symbols with LaTeX math notation (`\eqn{}`) in the
  documentation for PDF manual generation.
- General cleanup of code and documentation for CRAN submission.

## 1.0.2 - 2025-07-13

- Bumped version to **1.0.2**.
- Added cross-references to imported package functions in documentation.
- Optimized internal functions for better efficiency.

## 1.0.1

- Previous version.
- Create and document all functions.
