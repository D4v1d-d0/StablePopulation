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

# Identify headers that match an accepted alias exactly or as a decorated
# header such as "mx (Fertility Rate)" or "Age (years)".
reconstruction_alias_matches <- function(normalized_names, accepted_names) {
  accepted_normalized <- unique(normalize_reconstruction_name(accepted_names))

  vapply(normalized_names, function(name) {
    any(
      name == accepted_normalized |
        startsWith(name, paste0(accepted_normalized, "_")) |
        endsWith(name, paste0("_", accepted_normalized))
    )
  }, logical(1))
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

  matches <- which(
    reconstruction_alias_matches(
      normalized_names = normalized_names,
      accepted_names = accepted_names
    )
  )

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
      "fixed_<input sheet>",
      "admissible_<input sheet>",
      "scenarios_<input sheet>"
    ),
    contents = c(
      "Processing record, recognized columns, route and output sheets for every input sheet.",
      "Candidate beta values and numerical diagnostics.",
      "All reconstructed survivorship candidates for a scan route.",
      "Selected profile, observed and reconstructed survivorship, and derived demographic quantities R, D, D_relative and B.",
      "Profile reconstructed from one fixed beta value supplied in the input sheet.",
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

# Extract an optional fixed beta value from a recognized input column.
extract_reconstruction_fixed_beta <- function(
  raw_beta,
  source_rows,
  sheet_name,
  column_name
) {
  beta_missing <- is_reconstruction_blank(raw_beta)

  if (all(beta_missing)) {
    return(NULL)
  }

  beta_rows <- source_rows[!beta_missing]
  beta_values <- coerce_reconstruction_numeric(raw_beta[!beta_missing])

  if (any(!is.finite(beta_values))) {
    stop(
      "Sheet '", sheet_name, "' has non-numeric beta values in Excel row(s) ",
      paste(beta_rows[!is.finite(beta_values)], collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (any(beta_values <= 0)) {
    stop(
      "Sheet '", sheet_name, "' has non-positive beta values in Excel row(s) ",
      paste(beta_rows[beta_values <= 0], collapse = ", "), ".",
      call. = FALSE
    )
  }

  scale <- max(1, max(abs(beta_values)))
  if ((max(beta_values) - min(beta_values)) >
      sqrt(.Machine$double.eps) * scale) {
    stop(
      "Sheet '", sheet_name, "' has multiple beta values in column '",
      column_name,
      "'. A fixed-beta input column must contain one positive value, with optional blank cells below it.",
      call. = FALSE
    )
  }

  beta_values[1L]
}

# Prepare a recognized data sheet, retaining user labels but calculating on
# consecutive internal class indices.
prepare_reconstruction_sheet <- function(
  data,
  sheet_name,
  age_column = NULL,
  fertility_column = NULL,
  survivorship_column = NULL,
  beta_column = NULL
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
      "age", "edad", "age_year", "age_years", "edad_ano", "edad_anos",
      "edad_anio", "edad_anios", "age_class", "age_classes",
      "clase_edad", "clase_de_edad", "age_group", "grupo_edad"
    ),
    fertility = c(
      "mx", "m_x", "fertility_rates", "fertility_rate", "fertility",
      "female_fertility", "female_fertility_rate", "fecundity",
      "fecundity_rate", "fecundity_rates", "female_fecundity",
      "female_fecundity_rate", "fecundidad", "tasa_fecundidad",
      "tasa_de_fecundidad"
    ),
    survivorship = c(
      "lx_observed", "l_x_observed", "lx", "l_x", "survivorship",
      "survival", "supervivencia", "supervivencia_observada",
      "supervivencia_obs", "lx_obs", "l_x_obs"
    ),
    beta = c(
      "beta", "weibull_beta", "beta_weibull", "beta_parameter",
      "shape", "shape_parameter", "weibull_shape"
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

  beta_match <- find_reconstruction_column(
    data_names = data_names,
    accepted_names = aliases$beta,
    requested_name = beta_column,
    role = "beta",
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

  fixed_beta <- NULL
  beta_source_name <- NA_character_
  if (length(beta_match) > 0L) {
    fixed_beta <- extract_reconstruction_fixed_beta(
      raw_beta = data[[beta_match]],
      source_rows = source_rows,
      sheet_name = sheet_name,
      column_name = data_names[beta_match]
    )

    if (!is.null(fixed_beta)) {
      beta_source_name <- data_names[beta_match]
    }
  }

  list(
    is_data_sheet = TRUE,
    age_index = age_index,
    age_labels = unname(age_labels),
    fertility_rates = fertility_rates,
    lx_observed = lx_observed,
    fixed_beta = fixed_beta,
    source_columns = list(
      age = age_source_name,
      fertility = data_names[fertility_match],
      survivorship = survivorship_source_name,
      beta = beta_source_name
    )
  )
}
