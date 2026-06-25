#' Find Root Directory of StablePopulation
#'
#' This internal function searches for the root directory of the \code{StablePopulation}
#' project by looking for a folder named \code{StablePopulation} in the current
#' or parent directories. It is used internally to locate project-specific files.
#'
#' @return A character string with the full path to the \code{StablePopulation}
#'   directory if found. If not found, an error is raised.
#' @keywords internal
find_stablepopulations_root <- function() {
  # Name of the directory to search for.
  target_dir <- "StablePopulation"

  # Start with the current directory.
  current_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

  # Step 1: Check immediate subdirectories of the current directory.
  immediate_subdirs <- list.dirs(
    current_dir,
    recursive = FALSE,
    full.names = TRUE
  )

  for (subdir in immediate_subdirs) {
    if (basename(subdir) == target_dir) {
      return(subdir)
    }
  }

  # Step 2: Traverse upwards, checking the target directory directly and
  # among descendants of each parent directory.
  while (TRUE) {
    potential_path <- file.path(current_dir, target_dir)

    if (dir.exists(potential_path)) {
      return(potential_path)
    }

    descendants <- list.dirs(
      current_dir,
      recursive = TRUE,
      full.names = TRUE
    )

    for (descendant in descendants) {
      if (basename(descendant) == target_dir) {
        return(descendant)
      }
    }

    parent_dir <- dirname(current_dir)

    if (parent_dir == current_dir) {
      stop(
        "Cannot find the 'StablePopulation' directory in the current or parent directories."
      )
    }

    current_dir <- parent_dir
  }
}

#' Run Analysis on Excel Data and Export Results
#'
#' This historical function reads fertility rates and one pre-defined \code{beta}
#' value from each worksheet of \code{inst/extdata/Input_Data.xlsx}. It calculates
#' the corresponding \code{alpha} under \eqn{R_0 = 1} and writes one results
#' workbook per worksheet/species.
#'
#' @details
#' The historical Excel layout is fixed:
#' \itemize{
#'   \item column B contains the fertility schedule \eqn{m_x}, excluding its header;
#'   \item cell C2 contains the pre-defined Weibull shape parameter \code{beta};
#'   \item the first row is interpreted as a header row.
#' }
#'
#' This function is retained for compatibility with StablePopulation 1.0.3 and
#' with the workflow described in the ecoinformatics note. New scan and
#' observed-survivorship workflows are available separately through
#' \code{run_reconstruction_excel()}.
#'
#' @seealso
#' \code{\link[readxl]{excel_sheets}},
#' \code{\link[readxl]{read_excel}},
#' \code{\link[openxlsx]{createWorkbook}},
#' \code{\link[openxlsx]{addWorksheet}},
#' \code{\link[openxlsx]{writeData}},
#' \code{\link[openxlsx]{saveWorkbook}}
#'
#' @return No return value. Called for side effects: reading the bundled project
#'   workbook, writing one result workbook per worksheet, and printing messages.
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @export
run_analysis <- function() {
  # Detect the root directory of the project.
  root <- find_stablepopulations_root()

  # Construct the path to the historical input file.
  input_file <- file.path(root, "inst", "extdata", "Input_Data.xlsx")

  if (!file.exists(input_file)) {
    stop("The input file 'Input_Data.xlsx' does not exist in the expected location.")
  }

  # Get all sheet names from the input Excel file.
  sheet_names <- readxl::excel_sheets(input_file)

  # Iterate over each sheet in the input file (each species).
  for (sheet in sheet_names) {
    data <- readxl::read_excel(input_file, sheet = sheet, col_names = FALSE)

    # Historical layout: fertility in column B, beta in cell C2.
    fertility_rates <- as.numeric(data[[2]][-1])
    beta_value <- as.numeric(data[[3]][2])

    alpha_value <- find_alphas(beta_value, fertility_rates, tol = 1e-22)
    result <- calculate_population(alpha_value, beta_value, fertility_rates)

    population_matrix <- result$population

    if (is.null(population_matrix)) {
      stop(
        "The 'population_matrix' is null. Verify the result of find_alphas()."
      )
    }

    n <- length(population_matrix)
    alpha_row <- c(alpha_value, rep(NA, n - 1L))
    beta_row <- c(beta_value, rep(NA, n - 1L))

    result_matrix <- cbind(population_matrix, alpha_row, beta_row)
    result_matrix <- rbind(
      c("Population Profile", "alpha", "beta"),
      result_matrix
    )

    # Create a separate workbook for this species.
    workbook <- openxlsx::createWorkbook()
    sheet_name <- paste("Sheet", 1L)

    openxlsx::addWorksheet(workbook, sheet_name)
    openxlsx::writeData(
      workbook,
      sheet_name,
      result_matrix,
      colNames = FALSE
    )

    output_file <- file.path(
      root,
      "inst",
      "extdata",
      paste0(sheet, "_results.xlsx")
    )

    openxlsx::saveWorkbook(workbook, output_file, overwrite = TRUE)
    message(paste0("Results for ", sheet, " saved to ", output_file, "."))
  }

  message("Analysis complete for all species.")
}
