#' Reconstruct stable-population profiles from an Excel workbook
#'
#' @description
#' Reads one or more Excel sheets containing a fertility schedule and an
#' optional observed-survivorship profile. The default workbook is designed for
#' direct use: it starts with an overview and then provides one result sheet per
#' processed input sheet. A full audit workbook can be requested when candidate
#' and profile diagnostics are needed.
#'
#' @param input_file Optional path to an existing Excel workbook. When omitted or
#'   \code{NULL} in an interactive R session, a file chooser opens. It must be
#'   supplied in non-interactive use.
#' @param output_file Optional path for the results workbook. When omitted,
#'   creates \code{<input name>_StablePopulation.xlsx} beside the input file.
#' @param mode One of \code{"auto"}, \code{"scan"}, \code{"select"}, or
#'   \code{"fixed"}. With \code{"auto"}, the function uses \code{"select"}
#'   when observed survivorship is present, \code{"fixed"} when a recognized
#'   beta column contains one positive value, and \code{"scan"} otherwise.
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
#' @param beta_column Optional explicit fixed-beta column name.
#' @param tol Positive numerical tolerance for root finding.
#' @param r0_tolerance Positive numerical tolerance for the \eqn{R_0 = 1}
#'   check.
#' @param output_detail One of \code{"standard"} or \code{"full"}. The
#'   default \code{"standard"} output contains only \code{Overview} and one
#'   \code{Result_<sheet>} worksheet per processed input sheet. The
#'   \code{"full"} output additionally contains \code{Candidates_<sheet>}
#'   for select and scan routes, \code{Profiles_<sheet>} for scan routes, and
#'   a final \code{Metadata} worksheet.
#'
#' @details
#' Headings are matched case-insensitively and may include descriptive text or
#' units. For example, \code{Age (years)}, \code{mx (Fertility Rate)}, and
#' \code{lx (Survivorship)} are recognized automatically. Fertility aliases
#' include \code{mx}, \code{m_x}, \code{fertility}, \code{fertility_rates},
#' \code{fecundity}, and \code{fecundidad}. Observed-survivorship aliases
#' include \code{lx}, \code{l_x}, \code{lx_observed},
#' \code{l_x_observed}, \code{survivorship}, \code{survival}, and
#' \code{supervivencia}. Age aliases include \code{age}, \code{edad},
#' \code{age_class}, and \code{clase_edad}. Fixed-beta aliases include
#' \code{beta}, \code{weibull_beta}, \code{shape}, and
#' \code{shape_parameter}.
#'
#' A recognized age column is retained as an output label, whereas all
#' calculations use the internal consecutive class index \code{0, 1, ..., n - 1}.
#' Fully blank rows are ignored. Missing or non-numeric fertility values inside a
#' data table stop the run with the affected Excel row numbers; they are never
#' silently removed.
#'
#' A select or fixed-beta route produces one reconstructed profile. A scan route
#' does not choose one arbitrary beta: its result sheet lists the stable
#' candidates, or terminal-window-admissible candidates when a terminal window
#' is supplied.
#'
#' The Excel workflow analyses fertility exactly as supplied; it never applies
#' implicit fertility normalisation. When a schedule must be rescaled relative
#' to a reference survivorship profile, use [normalize_fertility()] in R first
#' and save the resulting fertility values to the workbook.
#'
#' @return Invisibly, a list with \code{output_file}, one result object per
#'   processed input sheet, and a metadata table that also records skipped
#'   sheets. Metadata is returned regardless of \code{output_detail}.
#'
#' @examples
#' \dontrun{
#' # Opens a file chooser in an interactive R session and creates the
#' # standard output workbook.
#' if (interactive()) {
#'   run_reconstruction_excel()
#' }
#'
#' # Creates demography_StablePopulation.xlsx beside demography.xlsx.
#' run_reconstruction_excel("demography.xlsx")
#'
#' # Request technical candidate and profile diagnostics.
#' run_reconstruction_excel("demography.xlsx", output_detail = "full")
#'
#' # Use explicit column names when an input workbook has custom headings.
#' run_reconstruction_excel(
#'   input_file = "demography.xlsx",
#'   output_file = "results.xlsx",
#'   sheets = "Ovis_dalli",
#'   age_column = "Age class",
#'   fertility_column = "Female fertility",
#'   survivorship_column = "Observed survivorship",
#'   beta_column = "Beta"
#' )
#' }
#'
#' @export
run_reconstruction_excel <- function(
  input_file = NULL,
  output_file = NULL,
  mode = c("auto", "scan", "select", "fixed"),
  beta_values = seq(0.05, 3.00, by = 0.05),
  terminal_window = NULL,
  sheets = NULL,
  skip_non_data = TRUE,
  age_column = NULL,
  fertility_column = NULL,
  survivorship_column = NULL,
  beta_column = NULL,
  tol = 1e-12,
  r0_tolerance = 1e-8,
  output_detail = c("standard", "full")
) {
  mode <- match.arg(mode)
  output_detail <- match.arg(output_detail)

  if (is.null(input_file)) {
    input_file <- choose_reconstruction_input_file()
  }

  if (!is.character(input_file) || length(input_file) != 1L ||
      is.na(input_file) || !nzchar(trimws(input_file))) {
    stop("'input_file' must identify an existing file.", call. = FALSE)
  }
  input_file <- trimws(input_file)

  if (!file.exists(input_file)) {
    stop("'input_file' must identify an existing file.", call. = FALSE)
  }

  if (is.null(output_file)) {
    output_file <- default_reconstruction_output_file(input_file)
  }
  if (!is.character(output_file) || length(output_file) != 1L ||
      is.na(output_file) || !nzchar(trimws(output_file))) {
    stop("'output_file' must be NULL or one non-empty path.", call. = FALSE)
  }
  output_file <- trimws(output_file)
  if (!grepl("\\.xlsx$", output_file, ignore.case = TRUE)) {
    output_file <- paste0(output_file, ".xlsx")
  }

  if (!is.logical(skip_non_data) || length(skip_non_data) != 1L ||
      is.na(skip_non_data)) {
    stop("'skip_non_data' must be TRUE or FALSE.", call. = FALSE)
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
  beta_column <- validate_reconstruction_column_name(beta_column, "beta_column")

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
    stop("'sheets' must contain valid workbook sheet names.", call. = FALSE)
  }
  sheets <- unique(sheets)

  output_directory <- dirname(output_file)
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }

  input_normalised <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
  output_normalised <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
  if (identical(tolower(input_normalised), tolower(output_normalised))) {
    stop("'output_file' cannot overwrite 'input_file'.", call. = FALSE)
  }

  workbook <- openxlsx::createWorkbook()
  initialize_reconstruction_overview(workbook, output_detail = output_detail)
  used_names <- "Overview"

  results <- list()
  metadata_rows <- list()

  for (sheet in sheets) {
    data <- readxl::read_excel(
      input_file,
      sheet = sheet,
      .name_repair = "unique_quiet"
    )
    prepared <- prepare_reconstruction_sheet(
      data = data,
      sheet_name = sheet,
      age_column = age_column,
      fertility_column = fertility_column,
      survivorship_column = survivorship_column,
      beta_column = beta_column
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
        beta_column = NA_character_,
        fixed_beta = NA_real_,
        result_sheet = NA_character_,
        candidates_sheet = NA_character_,
        profiles_sheet = NA_character_,
        summary_sheet = NA_character_,
        profile_sheet = NA_character_,
        scenario_sheet = NA_character_,
        selected_beta = NA_real_,
        selected_alpha = NA_real_,
        selected_RMSE = NA_real_,
        selected_R0 = NA_real_,
        selected_lx_terminal = NA_real_,
        overview_beta = "",
        overview_alpha = "",
        overview_rmse = "",
        overview_R0 = "",
        overview_candidate_status = "",
        overview_lx_terminal = "",
        note = prepared$reason,
        stringsAsFactors = FALSE
      )

      message("Skipped sheet '", sheet, "': ", prepared$reason)
      next
    }

    route <- if (identical(mode, "auto")) {
      if (!is.null(prepared$lx_observed)) {
        "select"
      } else if (!is.null(prepared$fixed_beta)) {
        "fixed"
      } else {
        "scan"
      }
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

    if (identical(route, "fixed") && is.null(prepared$fixed_beta)) {
      stop(
        "Sheet '", sheet,
        "' needs one positive beta value for mode = 'fixed'.",
        call. = FALSE
      )
    }

    result_sheet <- make_reconstruction_sheet_name("Result", sheet, used_names)
    used_names <- c(used_names, result_sheet)
    candidates_sheet <- NA_character_
    profiles_sheet <- NA_character_
    note <- NA_character_
    selected_beta <- NA_real_
    selected_alpha <- NA_real_
    selected_RMSE <- NA_real_
    selected_R0 <- NA_real_
    selected_lx_terminal <- NA_real_
    overview_beta <- ""
    overview_alpha <- ""
    overview_rmse <- ""
    overview_R0 <- ""
    overview_candidate_status <- ""
    overview_lx_terminal <- ""

    if (identical(route, "select") && !is.null(prepared$fixed_beta)) {
      note <- "Observed survivorship was available, so the fixed beta input was not used."
    }
    if (identical(route, "scan") && !is.null(prepared$fixed_beta)) {
      note <- "A fixed beta input was detected but was not used because mode = 'scan'."
    }
    if (identical(route, "fixed") && !is.null(terminal_window)) {
      note <- "terminal_window is not used for a fixed-beta reconstruction."
    }

    if (identical(route, "select")) {
      result <- select_beta(
        fertility_rates = prepared$fertility_rates,
        lx_observed = prepared$lx_observed,
        beta_values = beta_values,
        tol = tol,
        r0_tolerance = r0_tolerance
      )

      result_note <- if (is.na(note)) NULL else note
      if (isTRUE(result$beta_at_boundary)) {
        result_note <- paste(
          c(result_note, result$beta_boundary_note),
          collapse = " "
        )
        note <- result_note
      }

      demographic_profile <- derive_demographic_profile(
        lx = result$best_lx,
        fertility_rates = prepared$fertility_rates,
        tolerance = r0_tolerance
      )
      result_output <- make_reconstruction_profile_output(
        age_labels = prepared$age_labels,
        fertility_rates = prepared$fertility_rates,
        lx_reconstructed = result$best_lx,
        demographic_profile = demographic_profile,
        lx_observed = prepared$lx_observed
      )
      write_reconstruction_table(
        workbook,
        result_sheet,
        result_output,
        note = result_note
      )

      if (identical(output_detail, "full")) {
        candidates_sheet <- make_reconstruction_sheet_name(
          "Candidates",
          sheet,
          used_names
        )
        used_names <- c(used_names, candidates_sheet)
        write_reconstruction_table(workbook, candidates_sheet, result$results)
      }

      selected_beta <- result$best_beta
      selected_alpha <- result$best_alpha
      selected_RMSE <- result$best_RMSE
      selected_R0 <- result$best_R0
      selected_lx_terminal <- result$best_lx[length(result$best_lx)]
      overview_beta <- format_reconstruction_number(selected_beta)
      overview_alpha <- format_reconstruction_number(selected_alpha)
      overview_rmse <- format_reconstruction_number(selected_RMSE)
      overview_R0 <- format_reconstruction_number(selected_R0)
      overview_lx_terminal <- format_reconstruction_number(selected_lx_terminal)

    } else if (identical(route, "fixed")) {
      result <- reconstruct_population(
        fertility_rates = prepared$fertility_rates,
        beta = prepared$fixed_beta,
        tol = tol,
        r0_tolerance = r0_tolerance
      )

      demographic_profile <- derive_demographic_profile(
        lx = result$lx,
        fertility_rates = prepared$fertility_rates,
        tolerance = r0_tolerance
      )
      result_output <- make_reconstruction_profile_output(
        age_labels = prepared$age_labels,
        fertility_rates = prepared$fertility_rates,
        lx_reconstructed = result$lx,
        demographic_profile = demographic_profile,
        lx_observed = prepared$lx_observed
      )
      write_reconstruction_table(workbook, result_sheet, result_output)

      selected_beta <- result$beta
      selected_alpha <- result$alpha
      selected_R0 <- result$R0
      selected_lx_terminal <- result$lx[length(result$lx)]
      overview_beta <- format_reconstruction_number(selected_beta)
      overview_alpha <- format_reconstruction_number(selected_alpha)
      overview_R0 <- format_reconstruction_number(selected_R0)
      overview_lx_terminal <- format_reconstruction_number(selected_lx_terminal)

    } else {
      result <- scan_beta(
        fertility_rates = prepared$fertility_rates,
        beta_values = beta_values,
        tol = tol,
        r0_tolerance = r0_tolerance,
        terminal_window = terminal_window
      )

      n_stable <- sum(result$summary$stable)
      scan_note <- if (n_stable == 0L) {
        paste(
          "No numerically stable candidate was obtained for the requested beta range.",
          "No scenario profile can be reported. Review the fertility schedule",
          "or adjust beta_values."
        )
      } else if (is.null(terminal_window)) {
        paste(
          "No single profile was selected because this sheet has neither",
          "observed survivorship nor a fixed beta. The table lists all stable",
          "candidate scenarios."
        )
      } else if (nrow(result$admissible_summary) > 0L) {
        paste(
          "No single profile was selected. The table lists candidates whose",
          "terminal survivorship lies inside the requested terminal window."
        )
      } else {
        paste(
          "No candidate satisfied the requested terminal window. The table",
          "lists all numerically stable candidates."
        )
      }
      if (!is.na(note)) {
        scan_note <- paste(scan_note, note)
      }

      result_output <- make_reconstruction_scan_result_output(result)
      write_reconstruction_table(
        workbook,
        result_sheet,
        result_output,
        title = "Candidate scenarios",
        note = scan_note
      )

      if (identical(output_detail, "full")) {
        candidates_sheet <- make_reconstruction_sheet_name(
          "Candidates",
          sheet,
          used_names
        )
        used_names <- c(used_names, candidates_sheet)
        write_reconstruction_table(workbook, candidates_sheet, result$summary)

        profiles_sheet <- make_reconstruction_sheet_name(
          "Profiles",
          sheet,
          used_names
        )
        used_names <- c(used_names, profiles_sheet)
        profiles_output <- data.frame(
          `Age index` = prepared$age_index,
          Age = prepared$age_labels,
          `Fertility (mx)` = prepared$fertility_rates,
          result$profiles,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        write_reconstruction_table(workbook, profiles_sheet, profiles_output)
      }

      overview_beta <- paste0(
        format_reconstruction_number(min(beta_values)),
        " to ",
        format_reconstruction_number(max(beta_values)),
        " (", length(beta_values), " candidates)"
      )
      overview_candidate_status <- make_reconstruction_scan_candidate_status(result)
      if (is.na(note)) {
        note <- scan_note
      }
    }

    results[[sheet]] <- result
    metadata_rows[[length(metadata_rows) + 1L]] <- data.frame(
      sheet = sheet,
      status = "processed",
      route = route,
      n_age = length(prepared$fertility_rates),
      n_beta = if (identical(route, "fixed")) 1L else length(beta_values),
      age_column = prepared$source_columns$age,
      fertility_column = prepared$source_columns$fertility,
      survivorship_column = prepared$source_columns$survivorship,
      beta_column = prepared$source_columns$beta,
      fixed_beta = if (is.null(prepared$fixed_beta)) NA_real_ else prepared$fixed_beta,
      result_sheet = result_sheet,
      candidates_sheet = candidates_sheet,
      profiles_sheet = profiles_sheet,
      summary_sheet = candidates_sheet,
      profile_sheet = result_sheet,
      scenario_sheet = NA_character_,
      selected_beta = selected_beta,
      selected_alpha = selected_alpha,
      selected_RMSE = selected_RMSE,
      selected_R0 = selected_R0,
      selected_lx_terminal = selected_lx_terminal,
      overview_beta = overview_beta,
      overview_alpha = overview_alpha,
      overview_rmse = overview_rmse,
      overview_R0 = overview_R0,
      overview_candidate_status = overview_candidate_status,
      overview_lx_terminal = overview_lx_terminal,
      note = note,
      stringsAsFactors = FALSE
    )

    message("Processed sheet '", sheet, "' through the ", route, " route.")
  }

  if (length(results) == 0L) {
    stop(
      paste(
        "No sheet with a recognized fertility column was processed.",
        "Use a fertility alias such as mx, m_x, fertility, fecundidad,",
        "or specify 'fertility_column'."
      ),
      call. = FALSE
    )
  }

  metadata <- do.call(rbind, metadata_rows)
  overview <- make_reconstruction_overview(metadata)
  write_reconstruction_overview(workbook, overview)

  if (identical(output_detail, "full")) {
    write_reconstruction_table(workbook, "Metadata", metadata)
  }

  openxlsx::saveWorkbook(workbook, output_file, overwrite = TRUE)

  message("Reconstruction complete. Output saved to ", output_file, ".")

  invisible(list(
    output_file = output_file,
    results = results,
    metadata = metadata
  ))
}
