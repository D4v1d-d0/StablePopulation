# NEWS for StablePopulation

## 1.1.0.9000 - 2026-06-25

Development version prepared during the UCM doctoral stage.

### Reconstruction routes and derived profiles

- Adds `reconstruct_population()` for one fixed-beta Weibull reconstruction
  under `R0 = 1`.
- Adds `scan_beta()` to generate constrained candidate profiles and, when a
  terminal-survivorship window is supplied, retain explicitly defined scenario
  profiles rather than select an arbitrary result.
- Adds `select_beta()` to select the constrained candidate with the smallest
  RMSE against observed survivorship, including an explicit warning and stored
  note when the selected beta lies on the boundary of the scanned range.
- Adds `derive_demographic_profile()` to derive the stable structure `R`, raw
  exit-by-death profile `D`, separately normalised `D_relative`, and
  conditional survival `B`.
- Adds `fit_weibull_free()` as a descriptive free two-parameter Weibull
  reference fit; it does not replace the constrained reconstruction route.
- Adds `normalize_fertility()` for the explicit rescaling of a fertility
  schedule relative to a reference survivorship profile before an analysis that
  is deliberately expressed at `R0 = 1`.

### Excel workflows and compatibility

- Adds `run_reconstruction_excel()` as the recommended external-workbook
  interface for fixed-beta reconstruction, beta scanning, observed-
  survivorship selection, and terminal-window scenarios. When called without
  `input_file` in an interactive R session, it opens the operating-system file
  chooser.
- Redesigns `run_reconstruction_excel()` output workbooks. The default
  `output_detail = "standard"` creates `Overview` followed by one
  `Result_<sheet>` worksheet per processed input sheet. `output_detail =
  "full"` additionally writes candidate and scan-profile sheets, followed by
  `Metadata` as the final worksheet.
- Prevents a scan route from presenting an arbitrary first beta as a selected
  result. Scan output now reports stable or terminal-window-admissible
  candidates and clearly states that no unique profile has been chosen.
- Extends `run_reconstruction_excel()` to recognise descriptive legacy headings
  such as `Age (years)`, `mx (Fertility Rate)`, and `lx (Survivorship)`. In
  `mode = "auto"`, a one-value `Beta` column is used for a fixed-beta route
  when observed survivorship is absent.
- Restores and preserves `run_analysis()` exactly as the historical
  StablePopulation 1.0.3 source-tree Excel workflow: no function arguments,
  fertility in column B, one fixed beta in cell C2, and one output workbook per
  worksheet/species.

### Reliability and verification

- Adds shared input validation and a robust internal alpha solver that returns
  explicit diagnostics rather than silently treating a search endpoint as a
  valid root.
- Adds self-contained numerical regression tests against saved Model-V2022
  MATLAB outputs for *Castor fiber* and *Cervus elaphus*, including the
  historical terminal-window scenario range for *C. fiber*.
- Adds test coverage for the new reconstruction, selection, free-fit,
  normalisation, derived-profile, and Excel-workflow components.

### Documentation and citation

- Standardises source comments, user-facing messages, and package
  documentation in English.
- Clarifies the distinction between the route with observed survivorship
  (`select_beta()`) and the scenario route without it (`scan_beta()`), including
  the role of explicit fertility normalisation and the terminal window.
- Adds package-level help for choosing a route and interpreting `lx`, `R`, `D`,
  `D_relative`, and `B`.
- Updates the development citation to identify version 1.1.0.9000 as a GitHub
  development version while retaining the CRAN DOI for the published 1.0.3
  release.
- Adds an English vignette, `stablepopulation-workflow`, with a complete
  step-by-step workflow from input fertility and survivorship data to constrained
  Weibull reconstruction, derived demographic profiles, scenario scanning, and
  the Excel interface.
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
