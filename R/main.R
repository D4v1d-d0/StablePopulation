#' Find Root Directory of StablePopulation
#'
#' This internal function walks upwards from the current working directory until
#' it finds the source root of the \code{StablePopulation} package (identified by
#' a \code{DESCRIPTION} file with \code{Package: StablePopulation}). It is only
#' used as a fallback when the package is being run from source and the example
#' Excel file is not available through \code{system.file()}.
#'
#' @return A character string with the full path to the package root directory.
#' @keywords internal
find_stablepopulations_root <- function() {
  current_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

  while (TRUE) {
    description_file <- file.path(current_dir, "DESCRIPTION")

    if (file.exists(description_file)) {
      desc <- tryCatch(read.dcf(description_file), error = function(e) NULL)

      if (!is.null(desc) &&
          "Package" %in% colnames(desc) &&
          identical(desc[1, "Package"], "StablePopulation")) {
        return(current_dir)
      }
    }

    candidate_dir <- file.path(current_dir, "StablePopulation")
    candidate_desc <- file.path(candidate_dir, "DESCRIPTION")

    if (file.exists(candidate_desc)) {
      desc <- tryCatch(read.dcf(candidate_desc), error = function(e) NULL)

      if (!is.null(desc) &&
          "Package" %in% colnames(desc) &&
          identical(desc[1, "Package"], "StablePopulation")) {
        return(candidate_dir)
      }
    }

    parent_dir <- dirname(current_dir)
    if (parent_dir == current_dir) {
      stop(
        "Cannot find the StablePopulation package root. ",
        "Set the working directory to the package source or supply 'input_file' explicitly."
      )
    }

    current_dir <- parent_dir
  }
}

#' Run a beta sweep on Excel fertility data and export the predicted profiles
#'
#' This function reads fertility-rate data (\eqn{m_x}) from a multi-sheet Excel
#' file, performs a sweep of \eqn{\beta} values, computes for each \eqn{\beta}
#' the corresponding \eqn{\alpha} that satisfies \eqn{R_0 = \sum l_x m_x = 1},
#' and exports all predicted Weibull survival profiles (\eqn{l_x}) to a single
#' Excel workbook with one worksheet per input sheet.
#'
#' If \code{input_file} is not supplied, the function first looks for the bundled
#' example file via \code{system.file("extdata", "Input_Data.xlsx", package =
#' "StablePopulation")}. If that is not available, it falls back to the package
#' source tree and looks for \code{inst/extdata/Input_Data.xlsx}.
#'
#' @param input_file Optional path to the input Excel workbook. The workbook must
#'   contain one or more sheets, with ages in the first column and fertility
#'   rates in the second column. The first row is treated as a header row.
#' @param output_file Optional path to the output Excel workbook. If omitted, the
#'   file \code{Output_Profiles.xlsx} is written in the same directory as
#'   \code{input_file}.
#' @param beta_values Numeric vector of \eqn{\beta} values to evaluate. By
#'   default the function uses \code{seq(0.05, 3.00, by = 0.05)}.
#' @param tol Numeric tolerance passed to [find_alphas()]. Default:
#'   \code{1e-12}.
#'
#' @return Invisibly returns the full path to the output workbook.
#'
#' @details
#' The output workbook contains one sheet per input sheet. In each output sheet:
#' \itemize{
#'   \item the first row contains the evaluated \eqn{\beta} values,
#'   \item the second row contains the corresponding \eqn{\alpha} values,
#'   \item the third row contains the numerical check of \eqn{R_0},
#'   \item the remaining rows contain the predicted Weibull survival profiles
#'   (\eqn{l_x}) for each age.
#' }
#'
#' @seealso
#'   [find_alphas()], [calculate_population()], [readxl::read_excel()],
#'   [openxlsx::writeData()]
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @export
run_analysis <- function(
    input_file = NULL,
    output_file = NULL,
    beta_values = seq(0.05, 3.00, by = 0.05),
    tol = 1e-12
) {

  if (is.null(input_file)) {
    bundled_input <- system.file(
      "extdata",
      "Input_Data.xlsx",
      package = "StablePopulation"
    )

    if (nzchar(bundled_input) && file.exists(bundled_input)) {
      input_file <- bundled_input
    } else {
      root <- find_stablepopulations_root()
      input_file <- file.path(root, "inst", "extdata", "Input_Data.xlsx")
    }
  }

  if (!file.exists(input_file)) {
    stop(
      "The input file was not found. ",
      "Provide 'input_file' explicitly or place 'Input_Data.xlsx' in 'inst/extdata'."
    )
  }

  input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

  if (!is.numeric(beta_values) || length(beta_values) == 0L || any(!is.finite(beta_values))) {
    stop("'beta_values' must be a non-empty numeric vector of finite values.")
  }

  if (any(beta_values <= 0)) {
    stop("All values in 'beta_values' must be strictly positive.")
  }

  beta_values <- sort(unique(as.numeric(beta_values)))

  if (is.null(output_file)) {
    output_file <- file.path(dirname(input_file), "Output_Profiles.xlsx")
  }

  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  sheet_names <- readxl::excel_sheets(input_file)
  wb <- openxlsx::createWorkbook()

  for (sheet in sheet_names) {
    data <- readxl::read_excel(input_file, sheet = sheet, col_names = FALSE)

    ages <- suppressWarnings(as.numeric(data[[1]][-1]))
    fertility_rates <- suppressWarnings(as.numeric(data[[2]][-1]))

    valid_rows <- !is.na(fertility_rates)
    fertility_rates <- fertility_rates[valid_rows]
    ages <- ages[valid_rows]

    if (length(fertility_rates) == 0L) {
      warning(sprintf("Sheet '%s' contains no valid fertility data and was skipped.", sheet))
      next
    }

    n_ages <- length(fertility_rates)
    n_betas <- length(beta_values)

    alpha_values <- numeric(n_betas)
    r0_values <- numeric(n_betas)
    lx_matrix <- matrix(NA_real_, nrow = n_ages, ncol = n_betas)

    for (j in seq_along(beta_values)) {
      beta_j <- beta_values[j]
      alpha_j <- find_alphas(beta_j, fertility_rates, tol = tol)
      result_j <- calculate_population(alpha_j, beta_j, fertility_rates)

      alpha_values[j] <- alpha_j
      r0_values[j] <- result_j$births
      lx_matrix[, j] <- result_j$population
    }

    age_labels <- ifelse(
      !is.na(ages),
      paste0("age_", ages),
      paste0("age_index_", seq_len(n_ages) - 1L)
    )

    output_matrix <- rbind(beta_values, alpha_values, r0_values, lx_matrix)

    output_df <- data.frame(
      Parameter = c("beta", "alpha", "R0_check", age_labels),
      as.data.frame(output_matrix, check.names = FALSE),
      check.names = FALSE
    )

    colnames(output_df) <- c(
      "Parameter",
      paste0("beta_", formatC(beta_values, format = "f", digits = 2))
    )

    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, output_df, colNames = TRUE)

    message(sprintf(
      "Processed sheet '%s' with %d ages and %d beta values.",
      sheet, n_ages, n_betas
    ))
  }

  if (length(wb$sheet_names) == 0L) {
    stop("No output sheets were created. Check the contents of the input workbook.")
  }

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

  message(sprintf("Analysis complete. Output saved to %s", output_file))
  invisible(output_file)
}
