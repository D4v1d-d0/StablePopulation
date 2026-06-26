# Internal helpers for the extended Excel workflow.

# Convert column names to a robust comparison form while preserving the
# original labels for messages and output.
normalize_reconstruction_name <- function(x) {
  x <- trimws(as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

# Return TRUE for blank spreadsheet cells, including empty character cells.
is_reconstruction_blank <- function(x) {
  if (is.character(x) || is.factor(x)) {
    return(is.na(x) | trimws(as.character(x)) == "")
  }

  is.na(x)
}

# Identify rows that are completely blank across all columns.
reconstruction_blank_rows <- function(data) {
  if (nrow(data) == 0L) {
    return(logical(0))
  }

  blank_matrix <- vapply(
    data,
    is_reconstruction_blank,
    logical(nrow(data))
  )

  if (is.null(dim(blank_matrix))) {
    blank_matrix <- matrix(blank_matrix, ncol = 1L)
  }

  rowSums(blank_matrix) == ncol(data)
}

# Convert numeric spreadsheet columns, allowing a decimal comma only when a
# decimal point is absent. This keeps ordinary numeric Excel cells unchanged.
coerce_reconstruction_numeric <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  text <- trimws(as.character(x))
  comma_decimal <- grepl(",", text, fixed = TRUE) &
    !grepl(".", text, fixed = TRUE)
  text[comma_decimal] <- gsub(",", ".", text[comma_decimal], fixed = TRUE)
  suppressWarnings(as.numeric(text))
}

# Validate an optional explicit column name supplied by the user.
validate_reconstruction_column_name <- function(value, argument_name) {
  if (is.null(value)) {
    return(NULL)
  }

  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    stop(
      "'", argument_name, "' must be NULL or one non-empty column name.",
      call. = FALSE
    )
  }

  trimws(value)
}

# Identify one input column by an optional explicit name or by accepted aliases.
find_reconstruction_column <- function(
  data_names,
  accepted_names,
  requested_name = NULL,
  role,
  sheet_name
) {
  normalized_names <- normalize_reconstruction_name(data_names)

  if (!is.null(requested_name)) {
    requested_normalized <- normalize_reconstruction_name(requested_name)
    matches <- which(normalized_names == requested_normalized)

    if (length(matches) != 1L) {
      stop(
        "Sheet '", sheet_name, "' has no unique column named '",
        requested_name, "' for ", role, ".",
        call. = FALSE
      )
    }

    return(matches)
  }

  accepted_normalized <- normalize_reconstruction_name(accepted_names)
  matches <- which(normalized_names %in% accepted_normalized)

  if (length(matches) > 1L) {
    stop(
      "Sheet '", sheet_name, "' has multiple possible ", role,
      " columns (", paste(data_names[matches], collapse = ", "),
      "). Specify '", role, "_column' explicitly.",
      call. = FALSE
    )
  }

  matches
}

# Build legal and unique worksheet names.
make_reconstruction_sheet_name <- function(prefix, source_name, used_names) {
  base <- gsub("[^A-Za-z0-9_. -]", "_", paste0(prefix, "_", source_name))
  base <- substr(base, 1L, 31L)
  candidate <- base
  suffix <- 1L

  while (candidate %in% used_names) {
    suffix_text <- paste0("_", suffix)
    candidate <- paste0(substr(base, 1L, 31L - nchar(suffix_text)), suffix_text)
    suffix <- suffix + 1L
  }

  candidate
}

# Build the default output path beside the input workbook.
default_reconstruction_output_file <- function(input_file) {
  input_directory <- dirname(input_file)
  input_stem <- tools::file_path_sans_ext(basename(input_file))
  file.path(input_directory, paste0(input_stem, "_StablePopulation.xlsx"))
}

# Open the operating-system file chooser for the interactive Excel workflow.
choose_reconstruction_input_file <- function(is_interactive = interactive()) {
  if (!isTRUE(is_interactive)) {
    stop(
      "'input_file' must be supplied when run_reconstruction_excel() is used non-interactively.",
      call. = FALSE
    )
  }

  selected_file <- tryCatch(
    base::file.choose(new = FALSE),
    error = function(e) NA_character_
  )

  if (!is.character(selected_file) || length(selected_file) != 1L ||
      is.na(selected_file) || !nzchar(trimws(selected_file))) {
    stop(
      "No input workbook was selected.",
      call. = FALSE
    )
  }

  selected_file
}

# Write an output table with basic spreadsheet usability settings.
write_reconstruction_table <- function(workbook, sheet_name, data) {
  openxlsx::addWorksheet(workbook, sheet_name)
  openxlsx::writeData(workbook, sheet_name, data)

  if (ncol(data) > 0L) {
    openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(
      workbook,
      sheet_name,
      cols = seq_len(ncol(data)),
      widths = "auto"
    )
  }
}

# Write a brief guide into the result workbook.
write_reconstruction_readme <- function(
  workbook,
  input_file,
  output_file,
  mode,
  beta_values,
  terminal_window
) {
  run_information <- data.frame(
    item = c(
      "input_file",
      "output_file",
      "mode_requested",
      "beta_values",
      "terminal_window"
    ),
    value = c(
      input_file,
      output_file,
      mode,
      paste(beta_values, collapse = ", "),
      if (is.null(terminal_window)) {
        "NULL"
      } else {
        paste(terminal_window, collapse = ", ")
      }
    ),
    stringsAsFactors = FALSE
  )

  sheet_guide <- data.frame(
    sheet_pattern = c(
      "metadata_run",
      "summary_<input sheet>",
      "profiles_<input sheet>",
      "selected_<input sheet>",
      "admissible_<input sheet>",
      "scenarios_<input sheet>"
    ),
    contents = c(
      "Processing record, recognized columns, route and output sheets for every input sheet.",
      "Candidate beta values and numerical diagnostics.",
      "All reconstructed survivorship candidates for a scan route.",
      "Selected profile, observed and reconstructed survivorship, and derived demographic quantities R, D, D_relative and B.",
      "Candidates retained by the optional terminal-survivorship window.",
      "First and last terminal-admissible profiles when a terminal window is used."
    ),
    stringsAsFactors = FALSE
  )

  openxlsx::addWorksheet(workbook, "README")
  openxlsx::writeData(workbook, "README", run_information, startRow = 1L)
  openxlsx::writeData(
    workbook,
    "README",
    sheet_guide,
    startRow = nrow(run_information) + 3L
  )
  openxlsx::freezePane(workbook, "README", firstRow = TRUE)
  openxlsx::setColWidths(workbook, "README", cols = 1:2, widths = "auto")
}

# Prepare a recognized data sheet, retaining user labels but calculating on
# consecutive internal class indices.
prepare_reconstruction_sheet <- function(
  data,
  sheet_name,
  age_column = NULL,
  fertility_column = NULL,
  survivorship_column = NULL
) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)

  if (ncol(data) == 0L || nrow(data) == 0L) {
    return(list(
      is_data_sheet = FALSE,
      reason = "The sheet has no tabular data."
    ))
  }

  data_names <- names(data)
  aliases <- list(
    age = c(
      "age", "edad", "age_class", "age_classes", "clase_edad",
      "clase_de_edad", "age_group", "grupo_edad"
    ),
    fertility = c(
      "mx", "m_x", "fertility_rates", "fertility_rate", "fertility",
      "fecundity", "fecundidad", "tasa_fecundidad", "tasa_de_fecundidad"
    ),
    survivorship = c(
      "lx_observed", "l_x_observed", "lx", "l_x", "survivorship",
      "survival", "supervivencia", "supervivencia_observada",
      "supervivencia_obs", "lx_obs", "l_x_obs"
    )
  )

  fertility_match <- find_reconstruction_column(
    data_names = data_names,
    accepted_names = aliases$fertility,
    requested_name = fertility_column,
    role = "fertility",
    sheet_name = sheet_name
  )

  if (length(fertility_match) == 0L) {
    return(list(
      is_data_sheet = FALSE,
      reason = paste0(
        "No recognized fertility column. Accepted aliases include: ",
        paste(aliases$fertility, collapse = ", "), "."
      )
    ))
  }

  age_match <- find_reconstruction_column(
    data_names = data_names,
    accepted_names = aliases$age,
    requested_name = age_column,
    role = "age",
    sheet_name = sheet_name
  )

  survivorship_match <- find_reconstruction_column(
    data_names = data_names,
    accepted_names = aliases$survivorship,
    requested_name = survivorship_column,
    role = "survivorship",
    sheet_name = sheet_name
  )

  blank_rows <- reconstruction_blank_rows(data)
  if (all(blank_rows)) {
    stop(
      "Sheet '", sheet_name, "' has a recognized fertility column but no data rows.",
      call. = FALSE
    )
  }

  source_rows <- which(!blank_rows) + 1L
  data <- data[!blank_rows, , drop = FALSE]

  fertility_raw <- data[[fertility_match]]
  fertility_missing <- is_reconstruction_blank(fertility_raw)
  if (any(fertility_missing)) {
    stop(
      "Sheet '", sheet_name, "' has missing fertility values in Excel row(s) ",
      paste(source_rows[fertility_missing], collapse = ", "),
      ". Empty rows are allowed only when the entire row is blank.",
      call. = FALSE
    )
  }

  fertility_rates <- coerce_reconstruction_numeric(fertility_raw)
  fertility_invalid <- !is.finite(fertility_rates)
  if (any(fertility_invalid)) {
    stop(
      "Sheet '", sheet_name, "' has non-numeric fertility values in Excel row(s) ",
      paste(source_rows[fertility_invalid], collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (any(fertility_rates < 0)) {
    negative_rows <- which(fertility_rates < 0)
    stop(
      "Sheet '", sheet_name, "' has negative fertility values in Excel row(s) ",
      paste(source_rows[negative_rows], collapse = ", "), ".",
      call. = FALSE
    )
  }

  age_index <- seq.int(0L, length(fertility_rates) - 1L)
  if (length(age_match) == 0L) {
    age_labels <- age_index
    age_source_name <- NA_character_
  } else {
    age_labels <- data[[age_match]]
    age_missing <- is_reconstruction_blank(age_labels)
    if (any(age_missing)) {
      stop(
        "Sheet '", sheet_name, "' has missing age labels in Excel row(s) ",
        paste(source_rows[age_missing], collapse = ", "), ".",
        call. = FALSE
      )
    }
    age_source_name <- data_names[age_match]
  }

  lx_observed <- NULL
  survivorship_source_name <- NA_character_
  if (length(survivorship_match) > 0L) {
    survivorship_raw <- data[[survivorship_match]]
    survivorship_missing <- is_reconstruction_blank(survivorship_raw)

    if (!all(survivorship_missing)) {
      if (any(survivorship_missing)) {
        stop(
          "Sheet '", sheet_name, "' has incomplete observed survivorship in Excel row(s) ",
          paste(source_rows[survivorship_missing], collapse = ", "), ".",
          call. = FALSE
        )
      }

      lx_observed <- coerce_reconstruction_numeric(survivorship_raw)
      survivorship_invalid <- !is.finite(lx_observed)
      if (any(survivorship_invalid)) {
        stop(
          "Sheet '", sheet_name,
          "' has non-numeric observed survivorship in Excel row(s) ",
          paste(source_rows[survivorship_invalid], collapse = ", "), ".",
          call. = FALSE
        )
      }

      survivorship_source_name <- data_names[survivorship_match]
    }
  }

  list(
    is_data_sheet = TRUE,
    age_index = age_index,
    age_labels = unname(age_labels),
    fertility_rates = fertility_rates,
    lx_observed = lx_observed,
    source_columns = list(
      age = age_source_name,
      fertility = data_names[fertility_match],
      survivorship = survivorship_source_name
    )
  )
}

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
            formatC(first_candidate$beta, format = "fg", digits = 10, flag = "#")
          )
          last_name <- paste0(
            "lx_beta_",
            formatC(last_candidate$beta, format = "fg", digits = 10, flag = "#")
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
