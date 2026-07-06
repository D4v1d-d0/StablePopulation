
#' Reconstruct stable-population profiles from an Excel workbook
#'
#' @description
#' Reads one or more Excel sheets containing a fertility schedule and an
#' optional observed-survivorship profile. It automatically selects the
#' appropriate route, preserves user age labels in the output, creates a
#' results workbook beside the input by default, and records skipped non-data
#' sheets in metadata.
#'
#' @param input_file Optional path to an existing Excel workbook. When omitted or
#'   \code{NULL} in an interactive R session, a file chooser opens. It must be
#'   supplied in non-interactive use.
#' @param output_file Optional path for the results workbook. When omitted,
#'   creates \code{<input name>_StablePopulation.xlsx} beside the input file.
#' @param mode One of \code{"auto"}, \code{"scan"}, or \code{"select"}. With
#'   \code{"auto"}, the function uses \code{"select"} when an observed
#'   survivorship column is present and \code{"scan"} otherwise.
#' @param beta_values Positive Weibull shape values to scan.
#' @param terminal_window Optional terminal-survivorship window for the scan
#'   route.
#' @param sheets Optional workbook sheet names to process. By default, all
#'   sheets are inspected.
#' @param skip_non_data Logical. When \code{TRUE} and \code{sheets} is omitted,
#'   sheets without a recognized fertility column are recorded as skipped instead
#'   of stopping the run.
#' @param age_column Optional explicit age-column name.
#' @param fertility_column Optional explicit fertility-column name.
#' @param survivorship_column Optional explicit observed-survivorship column
#'   name.
#' @param tol Positive numerical tolerance for root finding.
#' @param r0_tolerance Positive numerical tolerance for the \eqn{R_0 = 1}
#'   check.
#'
#' @details
#' Fertility aliases include \code{mx}, \code{m_x}, \code{fertility},
#' \code{fertility_rates}, \code{fecundity}, \code{fecundidad},
#' \code{tasa_fecundidad}, and \code{tasa_de_fecundidad}. Observed-survivorship
#' aliases include \code{lx}, \code{l_x}, \code{lx_observed},
#' \code{l_x_observed}, \code{survivorship}, \code{survival},
#' \code{supervivencia}, and \code{supervivencia_observada}. Age aliases include
#' \code{age}, \code{edad}, \code{age_class}, and \code{clase_edad}.
#'
#' A recognized age column is retained as an output label, whereas all
#' calculations use the internal consecutive class index \code{0, 1, ..., n - 1}.
#' Fully blank rows are ignored. Missing or non-numeric fertility values inside a
#' data table stop the run with the affected Excel row numbers; they are never
#' silently removed.
#'
#' With a selected profile, the output includes the derived demographic
#' quantities \code{R}, \code{D}, \code{D_relative}, and \code{B}. With a scan
#' route, the output includes all candidate profiles and, when a terminal window
#' is supplied, the terminal-admissible candidates and their first and last
#' profiles.
#'
#' @return Invisibly, a list with \code{output_file}, one result object per
#'   processed input sheet, and a metadata table that also records skipped
#'   sheets.
#'
#' @examples
#' \dontrun{
#' # Opens a file chooser in an interactive R session.
#' if (interactive()) {
#'   run_reconstruction_excel()
#' }
#'
#' # Creates demography_StablePopulation.xlsx beside demography.xlsx
#' run_reconstruction_excel("demography.xlsx")
#'
#' # Use explicit column names when an input workbook has custom headings.
#' run_reconstruction_excel(
#'   input_file = "demography.xlsx",
#'   output_file = "results.xlsx",
#'   sheets = "Ovis_dalli",
#'   age_column = "Age class",
#'   fertility_column = "Female fertility",
#'   survivorship_column = "Observed survivorship"
#' )
#' }
#'
#' @export
run_reconstruction_excel <- function(
  input_file = NULL,
  output_file = NULL,
  mode = c("auto", "scan", "select"),
  beta_values = seq(0.05, 3.00, by = 0.05),
  terminal_window = NULL,
  sheets = NULL,
  skip_non_data = TRUE,
  age_column = NULL,
  fertility_column = NULL,
  survivorship_column = NULL,
  tol = 1e-12,
  r0_tolerance = 1e-8
) {
  mode <- match.arg(mode)

  if (is.null(input_file)) {
    input_file <- choose_reconstruction_input_file()
  }

  if (!is.character(input_file) || length(input_file) != 1L ||
      is.na(input_file) || !nzchar(trimws(input_file))) {
    stop(
      "'input_file' must identify an existing file.",
      call. = FALSE
    )
  }
  input_file <- trimws(input_file)

  if (!file.exists(input_file)) {
    stop(
      "'input_file' must identify an existing file.",
      call. = FALSE
    )
  }

  if (is.null(output_file)) {
    output_file <- default_reconstruction_output_file(input_file)
  }
  if (!is.character(output_file) || length(output_file) != 1L ||
      is.na(output_file) || !nzchar(trimws(output_file))) {
    stop(
      "'output_file' must be NULL or one non-empty path.",
      call. = FALSE
    )
  }
  output_file <- trimws(output_file)
  if (!grepl("\\.xlsx$", output_file, ignore.case = TRUE)) {
    output_file <- paste0(output_file, ".xlsx")
  }

  if (!is.logical(skip_non_data) || length(skip_non_data) != 1L ||
      is.na(skip_non_data)) {
    stop(
      "'skip_non_data' must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  age_column <- validate_reconstruction_column_name(age_column, "age_column")
  fertility_column <- validate_reconstruction_column_name(
    fertility_column,
    "fertility_column"
  )
  survivorship_column <- validate_reconstruction_column_name(
    survivorship_column,
    "survivorship_column"
  )

  beta_values <- validate_beta_values(beta_values)
  terminal_window <- validate_terminal_window(terminal_window)
  tol <- validate_positive_scalar(tol, "tol")
  r0_tolerance <- validate_positive_scalar(r0_tolerance, "r0_tolerance")

  available_sheets <- readxl::excel_sheets(input_file)
  inspect_all_sheets <- is.null(sheets)
  if (inspect_all_sheets) {
    sheets <- available_sheets
  }
  if (!is.character(sheets) || length(sheets) == 0L ||
      any(is.na(sheets)) || any(!sheets %in% available_sheets)) {
    stop(
      "'sheets' must contain valid workbook sheet names.",
      call. = FALSE
    )
  }
  sheets <- unique(sheets)

  output_directory <- dirname(output_file)
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }

  input_normalised <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
  output_normalised <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
  if (identical(tolower(input_normalised), tolower(output_normalised))) {
    stop(
      "'output_file' cannot overwrite 'input_file'.",
      call. = FALSE
    )
  }

  workbook <- openxlsx::createWorkbook()
  used_names <- "README"
  write_reconstruction_readme(
    workbook = workbook,
    input_file = input_file,
    output_file = output_file,
    mode = mode,
    beta_values = beta_values,
    terminal_window = terminal_window
  )

  results <- list()
  metadata_rows <- list()

  for (sheet in sheets) {
    data <- readxl::read_excel(input_file, sheet = sheet)
    prepared <- prepare_reconstruction_sheet(
      data = data,
      sheet_name = sheet,
      age_column = age_column,
      fertility_column = fertility_column,
      survivorship_column = survivorship_column
    )

    if (!isTRUE(prepared$is_data_sheet)) {
      can_skip <- inspect_all_sheets && isTRUE(skip_non_data) &&
        is.null(fertility_column)

      if (!can_skip) {
        stop(
          "Sheet '", sheet, "' cannot be processed: ", prepared$reason,
          call. = FALSE
        )
      }

      metadata_rows[[length(metadata_rows) + 1L]] <- data.frame(
        sheet = sheet,
        status = "skipped",
        route = NA_character_,
        n_age = 0L,
        n_beta = length(beta_values),
        age_column = NA_character_,
        fertility_column = NA_character_,
        survivorship_column = NA_character_,
        summary_sheet = NA_character_,
        profile_sheet = NA_character_,
        scenario_sheet = NA_character_,
        note = prepared$reason,
        stringsAsFactors = FALSE
      )

      message(
        "Skipped sheet '", sheet, "': ", prepared$reason
      )
      next
    }

    route <- if (identical(mode, "auto")) {
      if (is.null(prepared$lx_observed)) "scan" else "select"
    } else {
      mode
    }

    if (identical(route, "select") && is.null(prepared$lx_observed)) {
      stop(
        "Sheet '", sheet,
        "' needs observed survivorship for mode = 'select'.",
        call. = FALSE
      )
    }

    summary_sheet <- make_reconstruction_sheet_name("summary", sheet, used_names)
    used_names <- c(used_names, summary_sheet)
    profile_sheet <- NA_character_
    scenario_sheet <- NA_character_
    note <- NA_character_

    if (identical(route, "scan")) {
      result <- scan_beta(
        fertility_rates = prepared$fertility_rates,
        beta_values = beta_values,
        tol = tol,
        r0_tolerance = r0_tolerance,
        terminal_window = terminal_window
      )

      profiles_sheet <- make_reconstruction_sheet_name("profiles", sheet, used_names)
      used_names <- c(used_names, profiles_sheet)
      profile_sheet <- profiles_sheet

      profiles_output <- data.frame(
        age_index = prepared$age_index,
        age = prepared$age_labels,
        fertility_rates = prepared$fertility_rates,
        result$profiles,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      write_reconstruction_table(workbook, summary_sheet, result$summary)
      write_reconstruction_table(workbook, profiles_sheet, profiles_output)

      if (!is.null(terminal_window)) {
        admissible_sheet <- make_reconstruction_sheet_name(
          "admissible",
          sheet,
          used_names
        )
        used_names <- c(used_names, admissible_sheet)
        scenario_sheet <- admissible_sheet
        write_reconstruction_table(
          workbook,
          admissible_sheet,
          result$admissible_summary
        )

        if (!is.null(result$terminal_extremes)) {
          first_candidate <- result$terminal_extremes$first
          last_candidate <- result$terminal_extremes$last
          first_name <- paste0(
            "lx_beta_",
            formatC(first_candidate$beta, format = "fg", digits = 4)
          )
          last_name <- paste0(
            "lx_beta_",
            formatC(last_candidate$beta, format = "fg", digits = 4)
          )
          scenario_output <- data.frame(
            age_index = prepared$age_index,
            age = prepared$age_labels,
            fertility_rates = prepared$fertility_rates,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          scenario_output[[first_name]] <- first_candidate$lx
          scenario_output[[last_name]] <- last_candidate$lx

          terminal_sheet <- make_reconstruction_sheet_name(
            "scenarios",
            sheet,
            used_names
          )
          used_names <- c(used_names, terminal_sheet)
          scenario_sheet <- paste(admissible_sheet, terminal_sheet, sep = "; ")
          write_reconstruction_table(workbook, terminal_sheet, scenario_output)
        } else {
          note <- paste0(
            "No profile satisfied terminal_window = ",
            paste(terminal_window, collapse = ", "), "."
          )
        }
      }
    } else {
      result <- select_beta(
        fertility_rates = prepared$fertility_rates,
        lx_observed = prepared$lx_observed,
        beta_values = beta_values,
        tol = tol,
        r0_tolerance = r0_tolerance
      )

      selected_sheet <- make_reconstruction_sheet_name("selected", sheet, used_names)
      used_names <- c(used_names, selected_sheet)
      profile_sheet <- selected_sheet

      demographic_profile <- derive_demographic_profile(
        lx = result$best_lx,
        fertility_rates = prepared$fertility_rates,
        tolerance = r0_tolerance
      )

      selected_output <- data.frame(
        age_index = prepared$age_index,
        age = prepared$age_labels,
        fertility_rates = prepared$fertility_rates,
        lx_observed = prepared$lx_observed,
        lx_reconstructed = result$best_lx,
        residual = result$best_lx - prepared$lx_observed,
        squared_error = (result$best_lx - prepared$lx_observed) ^ 2,
        lxmx = result$best_lx * prepared$fertility_rates,
        R = demographic_profile$R,
        D = demographic_profile$D,
        D_relative = demographic_profile$D_relative,
        B = demographic_profile$B,
        stringsAsFactors = FALSE
      )

      write_reconstruction_table(workbook, summary_sheet, result$results)
      write_reconstruction_table(workbook, selected_sheet, selected_output)
    }

    results[[sheet]] <- result
    metadata_rows[[length(metadata_rows) + 1L]] <- data.frame(
      sheet = sheet,
      status = "processed",
      route = route,
      n_age = length(prepared$fertility_rates),
      n_beta = length(beta_values),
      age_column = prepared$source_columns$age,
      fertility_column = prepared$source_columns$fertility,
      survivorship_column = prepared$source_columns$survivorship,
      summary_sheet = summary_sheet,
      profile_sheet = profile_sheet,
      scenario_sheet = scenario_sheet,
      note = note,
      stringsAsFactors = FALSE
    )

    message(
      "Processed sheet '", sheet, "' through the ", route, " route."
    )
  }

  if (length(results) == 0L) {
    stop(
      "No sheet with a recognized fertility column was processed. Use a fertility alias such as mx, m_x, fertility, fecundidad, or specify 'fertility_column'.",
      call. = FALSE
    )
  }

  metadata <- do.call(rbind, metadata_rows)
  metadata_sheet <- make_reconstruction_sheet_name("metadata", "run", used_names)
  write_reconstruction_table(workbook, metadata_sheet, metadata)
  openxlsx::saveWorkbook(workbook, output_file, overwrite = TRUE)

  message(
    "Reconstruction complete. Output saved to ", output_file, "."
  )

  invisible(list(
    output_file = output_file,
    results = results,
    metadata = metadata
  ))
}
