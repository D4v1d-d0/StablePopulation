# Internal helper to identify an input column by accepted names.
# Funcion interna para identificar una columna de entrada por nombres admitidos.
find_reconstruction_column <- function(data_names, accepted_names) {
  lowered_names <- tolower(trimws(data_names))
  match(tolower(accepted_names), lowered_names, nomatch = 0L)
}

# Internal helper to build legal and unique worksheet names.
# Funcion interna para construir nombres de hoja validos y unicos.
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

#' Reconstruct stable-population profiles from an Excel workbook
#'
#' Reconstruye perfiles de poblacion estable desde un libro Excel
#'
#' @description
#' English: Reads one or more Excel sheets with a required fertility column and
#' an optional observed-survivorship column. It uses \code{scan_beta()} when
#' no observed \eqn{l_x} is supplied and \code{select_beta()} when it is.
#' This is the extended Excel workflow introduced after the historical
#' \code{run_analysis()} workflow; \code{run_analysis()} remains unchanged.
#'
#' Espanol: Lee una o varias hojas Excel con una columna obligatoria de
#' fecundidad y una columna opcional de supervivencia observada. Usa
#' \code{scan_beta()} cuando no se aporta \eqn{l_x} observado y
#' \code{select_beta()} cuando se aporta. Este es el flujo Excel ampliado
#' introducido despues del flujo historico \code{run_analysis()};
#' \code{run_analysis()} permanece sin cambios.
#'
#' @param input_file Path to an existing `.xlsx` workbook. / Ruta a un libro
#'   `.xlsx` existente.
#' @param output_file Path for the results `.xlsx` workbook. / Ruta del libro
#'   `.xlsx` de resultados.
#' @param mode One of \code{"auto"}, \code{"scan"}, or \code{"select"}.
#'   With \code{"auto"}, the function selects \code{"select"} when an observed
#'   survivorship column is present and otherwise uses \code{"scan"}. / Uno de
#'   \code{"auto"}, \code{"scan"} o \code{"select"}. Con \code{"auto"}, la
#'   funcion selecciona \code{"select"} cuando existe una columna de
#'   supervivencia observada y, en caso contrario, usa \code{"scan"}.
#' @param beta_values Positive Weibull shape values to scan. / Valores positivos
#'   del parametro de forma Weibull que se exploraran.
#' @param terminal_window Optional terminal window for the scan route. /
#'   Ventana terminal opcional para la via de barrido.
#' @param sheets Optional workbook sheet names to process. By default, all
#'   sheets are read. / Nombres opcionales de hojas del libro que se procesaran.
#'   Por defecto, se leen todas las hojas.
#' @param tol Positive numerical tolerance for root finding. / Tolerancia
#'   numerica positiva para la busqueda de la raiz.
#' @param r0_tolerance Positive numerical tolerance for the \eqn{R_0 = 1}
#'   check. / Tolerancia numerica positiva para la comprobacion \eqn{R_0 = 1}.
#'
#' @return Invisibly, a list with \code{output_file}, one result object per
#'   input sheet, and a metadata table. / Invisiblemente, una lista con
#'   \code{output_file}, un objeto de resultado por hoja de entrada y una tabla
#'   de metadatos.
#'
#' @details
#' Accepted fertility-column names are \code{mx}, \code{fertility_rates}, and
#' \code{fertility}. Accepted observed-survivorship names are
#' \code{lx_observed}, \code{lx}, and \code{survivorship}. An optional
#' \code{age} column is treated only as an input label: calculations always use
#' internal consecutive ages \code{0, 1, ..., n - 1}. /
#' Los nombres admitidos para la columna de fecundidad son \code{mx},
#' \code{fertility_rates} y \code{fertility}. Los nombres admitidos para la
#' supervivencia observada son \code{lx_observed}, \code{lx} y
#' \code{survivorship}. Una columna \code{age} opcional se trata solo como
#' etiqueta de entrada: los calculos siempre usan edades internas consecutivas
#' \code{0, 1, ..., n - 1}.
#'
#' @examples
#' \dontrun{
#' run_reconstruction_excel(
#'   input_file = "demography.xlsx",
#'   output_file = "reconstruction_results.xlsx",
#'   mode = "auto"
#' )
#' }
#'
#' @export
run_reconstruction_excel <- function(
  input_file,
  output_file,
  mode = c("auto", "scan", "select"),
  beta_values = seq(0.05, 3.00, by = 0.05),
  terminal_window = NULL,
  sheets = NULL,
  tol = 1e-12,
  r0_tolerance = 1e-8
) {
  mode <- match.arg(mode)

  if (!is.character(input_file) || length(input_file) != 1L ||
      !nzchar(input_file) || !file.exists(input_file)) {
    stop(
      "'input_file' must identify an existing file. / ",
      "'input_file' debe identificar un archivo existente.",
      call. = FALSE
    )
  }
  if (!is.character(output_file) || length(output_file) != 1L ||
      !nzchar(output_file)) {
    stop(
      "'output_file' must be one non-empty path. / ",
      "'output_file' debe ser una ruta no vacia.",
      call. = FALSE
    )
  }

  beta_values <- validate_beta_values(beta_values)
  terminal_window <- validate_terminal_window(terminal_window)
  tol <- validate_positive_scalar(tol, "tol")
  r0_tolerance <- validate_positive_scalar(r0_tolerance, "r0_tolerance")

  available_sheets <- readxl::excel_sheets(input_file)
  if (is.null(sheets)) {
    sheets <- available_sheets
  }
  if (!is.character(sheets) || length(sheets) == 0L ||
      any(!sheets %in% available_sheets)) {
    stop(
      "'sheets' must contain valid workbook sheet names. / ",
      "'sheets' debe contener nombres validos de hojas del libro.",
      call. = FALSE
    )
  }

  output_directory <- dirname(output_file)
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }

  workbook <- openxlsx::createWorkbook()
  used_names <- character(0)
  results <- vector("list", length(sheets))
  names(results) <- sheets
  metadata <- data.frame(
    sheet = character(0),
    route = character(0),
    n_age = integer(0),
    n_beta = integer(0),
    stringsAsFactors = FALSE
  )

  for (index in seq_along(sheets)) {
    sheet <- sheets[index]
    data <- readxl::read_excel(input_file, sheet = sheet)
    data_names <- names(data)

    fertility_match <- find_reconstruction_column(
      data_names,
      c("mx", "fertility_rates", "fertility")
    )
    fertility_match <- fertility_match[fertility_match > 0L]
    if (length(fertility_match) == 0L) {
      stop(
        "Sheet '", sheet, "' has no fertility column named mx, fertility_rates, or fertility. / ",
        "La hoja '", sheet, "' no tiene una columna de fecundidad llamada mx, fertility_rates o fertility.",
        call. = FALSE
      )
    }

    fertility_rates <- suppressWarnings(as.numeric(data[[fertility_match[1L]]]))
    retained_rows <- !is.na(fertility_rates)
    fertility_rates <- fertility_rates[retained_rows]

    observed_match <- find_reconstruction_column(
      data_names,
      c("lx_observed", "lx", "survivorship")
    )
    observed_match <- observed_match[observed_match > 0L]
    lx_observed <- NULL
    if (length(observed_match) > 0L) {
      candidate_lx <- suppressWarnings(as.numeric(data[[observed_match[1L]]]))
      candidate_lx <- candidate_lx[retained_rows]
      if (!all(is.na(candidate_lx))) {
        lx_observed <- candidate_lx
      }
    }

    route <- if (mode == "auto") {
      if (is.null(lx_observed)) "scan" else "select"
    } else {
      mode
    }

    if (identical(route, "select") && is.null(lx_observed)) {
      stop(
        "Sheet '", sheet, "' needs observed survivorship for mode = 'select'. / ",
        "La hoja '", sheet, "' necesita supervivencia observada para mode = 'select'.",
        call. = FALSE
      )
    }

    if (identical(route, "scan")) {
      result <- scan_beta(
        fertility_rates = fertility_rates,
        beta_values = beta_values,
        tol = tol,
        r0_tolerance = r0_tolerance,
        terminal_window = terminal_window
      )

      summary_sheet <- make_reconstruction_sheet_name(
        "summary", sheet, used_names
      )
      used_names <- c(used_names, summary_sheet)
      profiles_sheet <- make_reconstruction_sheet_name(
        "profiles", sheet, used_names
      )
      used_names <- c(used_names, profiles_sheet)

      openxlsx::addWorksheet(workbook, summary_sheet)
      openxlsx::writeData(workbook, summary_sheet, result$summary)
      openxlsx::addWorksheet(workbook, profiles_sheet)
      openxlsx::writeData(
        workbook,
        profiles_sheet,
        data.frame(age = result$age, result$profiles, check.names = FALSE)
      )
    } else {
      result <- select_beta(
        fertility_rates = fertility_rates,
        lx_observed = lx_observed,
        beta_values = beta_values,
        tol = tol,
        r0_tolerance = r0_tolerance
      )

      summary_sheet <- make_reconstruction_sheet_name(
        "summary", sheet, used_names
      )
      used_names <- c(used_names, summary_sheet)
      selected_sheet <- make_reconstruction_sheet_name(
        "selected", sheet, used_names
      )
      used_names <- c(used_names, selected_sheet)

      openxlsx::addWorksheet(workbook, summary_sheet)
      openxlsx::writeData(workbook, summary_sheet, result$results)
      openxlsx::addWorksheet(workbook, selected_sheet)
      openxlsx::writeData(workbook, selected_sheet, result$best_profile)
    }

    results[[index]] <- result
    metadata <- rbind(
      metadata,
      data.frame(
        sheet = sheet,
        route = route,
        n_age = length(fertility_rates),
        n_beta = length(beta_values),
        stringsAsFactors = FALSE
      )
    )

    message(
      "Processed sheet '", sheet, "' through the ", route,
      " route. / Hoja '", sheet, "' procesada mediante la via ", route, "."
    )
  }

  metadata_sheet <- make_reconstruction_sheet_name("metadata", "run", used_names)
  openxlsx::addWorksheet(workbook, metadata_sheet)
  openxlsx::writeData(workbook, metadata_sheet, metadata)
  openxlsx::saveWorkbook(workbook, output_file, overwrite = TRUE)

  message(
    "Reconstruction complete. Output saved to ", output_file,
    ". / Reconstruccion completada. Salida guardada en ", output_file, "."
  )

  invisible(list(output_file = output_file, results = results, metadata = metadata))
}
