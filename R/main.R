#' Find Root Directory of StablePopulation
#'
#' This internal function searches for the root directory of the `StablePopulation`
#' project by looking for a folder named `StablePopulation` in the current
#' or parent directories. It is used internally to locate project-specific files.
#'
#' @return The full path to the `StablePopulation` directory if found.
#' @keywords internal
find_stablepopulations_root <- function() {
  # Name of the directory to search for
  target_dir <- "StablePopulation"

  # Start with the current directory
  current_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

  # Step 1: Check immediate subdirectories of the current directory
  immediate_subdirs <- list.dirs(current_dir, recursive = FALSE, full.names = TRUE)
  for (subdir in immediate_subdirs) {
    if (basename(subdir) == target_dir) {
      return(subdir) # Found StablePopulation in subdirectory of the current directory
    }
  }

  # Step 2: Traverse upwards checking for the target directory directly
  while (TRUE) {
    # Check if the target directory exists directly in the current directory
    potential_path <- file.path(current_dir, target_dir)
    if (dir.exists(potential_path)) {
      return(potential_path) # Found StablePopulation in a parent directory
    }

    # Step 3: Check descendants (children, grandchildren, etc.) of the current directory
    descendants <- list.dirs(current_dir, recursive = TRUE, full.names = TRUE)
    for (descendant in descendants) {
      if (basename(descendant) == target_dir) {
        return(descendant) # Found StablePopulation among descendants
      }
    }

    # Move to the parent directory
    parent_dir <- dirname(current_dir)
    if (parent_dir == current_dir) { # Reached the root of the file system
      stop("Cannot find the 'StablePopulation' directory in the current or parent directories.")
    }
    current_dir <- parent_dir
  }
}



#' Run Analysis on Excel Data and Export Results
#'
#' This function reads fertility rate data and Beta value from an Excel file, processes it, and exports
#' the results to a new Excel file for the specie, including population matrices and calculated alpha/beta values.
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @export
run_analysis <- function() {

  # Detect the root directory of the project
  root <- find_stablepopulations_root()

  # Construct the path to the input file
  input_file <- file.path(root, "inst", "extdata", "Input_Data.xlsx")

  # Check if the input file exists
  if (!file.exists(input_file)) {
    stop("The input file 'Input_Data.xlsx' does not exist in the expected location.")
  }


  # Get all sheet names from the input file
  sheet_names <- readxl::excel_sheets(input_file)

  # Iterate over each sheet in the input file (each species)
  for (sheet in sheet_names) {
    # Read the specific sheet from the input Excel file
    data <- readxl::read_excel(input_file, sheet = sheet, col_names = FALSE)

    # Extract the second column of data, excluding the first row
    fertility_rates <- as.numeric(data[[2]][-1])

    # Extract the Beta value
    Beta_value <- as.numeric(data[[3]][2])

    # Run the `alphap_for_betas` function
    Alpha_value <- find_alphas(Beta_value, fertility_rates, tol = 1e-22)

    # Find population matrix
    result <- calculate_population(Alpha_value, Beta_value, fertility_rates)

    # Prepare population matrix and calculate sum 's'
    population_matrix <- result$population
    if (is.null(population_matrix)) {
      stop("The 'population_matrix' is null. Verify the result of alphap_for_betas.")
    }

    pop <- result$births
    #s <- pop debe dar 1

    # Prepare alpha and beta rows for output
    n <- length(population_matrix)
    alpha_row <- c(Alpha_value, rep(NA, n-1))
    #alpha_row <- t(alpha_row)
    beta_row <- c(Beta_value, rep(NA, n-1) )
    #beta_row <- t(beta_row)
    #population_matrix <- t(population_matrix)

    # Construct result matrix with labels and values
    result_matrix <- cbind(population_matrix, alpha_row, beta_row )
    result_matrix <- rbind(
      c("Population Profile","alpha", "beta"), # Profile label
      result_matrix
            # Population matrix
                                                # Beta value
    )


    # Create a new workbook for this species
    wb <- openxlsx::createWorkbook()

    # Add three identical worksheets to the workbook

      sheet_name <- paste("Sheet", 1)  # Name   sheet as "Sheet 1".
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, result_matrix, colNames = FALSE)


    # Define the output file name for this species
    output_file <- file.path(root, "inst", "extdata", paste0(sheet, "_results.xlsx"))

    # Save the Excel file for this species
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

    cat("Results for", sheet, "saved to", output_file, "\n")
  }

  cat("Analysis complete for all species.\n")
}
