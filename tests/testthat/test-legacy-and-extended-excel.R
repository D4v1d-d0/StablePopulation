test_that("run_analysis retains the StablePopulation 1.0.3 interface", {
  expect_null(formals(run_analysis))
})

test_that("standard Excel output contains Overview and Result sheets only", {
  input_file <- tempfile(fileext = ".xlsx")
  output_file <- tempfile(fileext = ".xlsx")

  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "scan_case")
  openxlsx::writeData(
    workbook,
    "scan_case",
    data.frame(mx = c(0, 0, 0.30, 0.75, 0.60, 0.20))
  )
  openxlsx::addWorksheet(workbook, "select_case")
  openxlsx::writeData(
    workbook,
    "select_case",
    data.frame(
      mx = c(0, 0, 0.30, 0.75, 0.60, 0.20),
      lx_observed = c(1, 0.93, 0.82, 0.67, 0.41, 0.15)
    )
  )
  openxlsx::saveWorkbook(workbook, input_file, overwrite = TRUE)

  result <- run_reconstruction_excel(
    input_file = input_file,
    output_file = output_file,
    beta_values = c(0.5, 1)
  )

  expect_true(file.exists(output_file))
  expect_identical(result$metadata$route, c("scan", "select"))

  output_sheets <- readxl::excel_sheets(output_file)
  expect_identical(output_sheets[1L], "Overview")
  expect_false(any(grepl("^(Metadata|Candidates|Profiles)_", output_sheets)))
  expect_true(all(result$metadata$result_sheet %in% output_sheets))

  scan_sheet <- result$metadata$result_sheet[result$metadata$sheet == "scan_case"]
  scan_output <- readxl::read_excel(output_file, sheet = scan_sheet, skip = 4L)
  expect_true(all(c("Beta", "Alpha", "Status") %in% names(scan_output)))
  expect_false("Reconstructed survivorship (lx)" %in% names(scan_output))

  select_sheet <- result$metadata$result_sheet[result$metadata$sheet == "select_case"]
  select_output <- readxl::read_excel(output_file, sheet = select_sheet)
  expect_true(all(c(
    "Age", "Fertility (mx)", "Observed survivorship (lx)",
    "Reconstructed survivorship (lx)", "Stable population proportion (R)",
    "Mortality profile (D)", "Relative mortality proportion (D_relative)",
    "Conditional survival (B)"
  ) %in% names(select_output)))
})

test_that("full Excel output includes diagnostics and ends with Metadata", {
  input_file <- tempfile(fileext = ".xlsx")
  output_file <- tempfile(fileext = ".xlsx")

  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "scan_case")
  openxlsx::writeData(
    workbook,
    "scan_case",
    data.frame(mx = c(0, 0, 0.30, 0.75, 0.60, 0.20))
  )
  openxlsx::addWorksheet(workbook, "select_case")
  openxlsx::writeData(
    workbook,
    "select_case",
    data.frame(
      mx = c(0, 0, 0.30, 0.75, 0.60, 0.20),
      lx_observed = c(1, 0.93, 0.82, 0.67, 0.41, 0.15)
    )
  )
  openxlsx::saveWorkbook(workbook, input_file, overwrite = TRUE)

  result <- run_reconstruction_excel(
    input_file = input_file,
    output_file = output_file,
    beta_values = c(0.5, 1),
    output_detail = "full"
  )

  output_sheets <- readxl::excel_sheets(output_file)
  expect_identical(output_sheets[1L], "Overview")
  expect_identical(tail(output_sheets, 1L), "Metadata")

  scan_row <- result$metadata[result$metadata$sheet == "scan_case", ]
  select_row <- result$metadata[result$metadata$sheet == "select_case", ]
  expect_true(scan_row$candidates_sheet %in% output_sheets)
  expect_true(scan_row$profiles_sheet %in% output_sheets)
  expect_true(select_row$candidates_sheet %in% output_sheets)
  expect_true(is.na(select_row$profiles_sheet))
})

test_that("run_reconstruction_excel recognizes aliases, preserves age labels, and skips non-data sheets", {
  input_file <- tempfile(fileext = ".xlsx")
  expected_output <- file.path(
    dirname(input_file),
    paste0(tools::file_path_sans_ext(basename(input_file)), "_StablePopulation.xlsx")
  )

  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "Spanish_aliases")
  openxlsx::writeData(
    workbook,
    "Spanish_aliases",
    data.frame(
      edad = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6"),
      fecundidad = c(0, 0, 0.30, 0.75, 0.60, 0.20),
      supervivencia = c(1, 0.93, 0.82, 0.67, 0.41, 0.15)
    )
  )
  openxlsx::addWorksheet(workbook, "Notes")
  openxlsx::writeData(workbook, "Notes", data.frame(note = "metadata only"))
  openxlsx::saveWorkbook(workbook, input_file, overwrite = TRUE)

  result <- run_reconstruction_excel(
    input_file = input_file,
    beta_values = c(0.5, 1)
  )

  expect_identical(result$output_file, expected_output)
  expect_true(file.exists(expected_output))

  processed <- result$metadata[result$metadata$sheet == "Spanish_aliases", ]
  skipped <- result$metadata[result$metadata$sheet == "Notes", ]
  expect_identical(processed$route, "select")
  expect_identical(processed$age_column, "edad")
  expect_identical(processed$fertility_column, "fecundidad")
  expect_identical(processed$survivorship_column, "supervivencia")
  expect_identical(skipped$status, "skipped")

  selected <- readxl::read_excel(expected_output, sheet = processed$result_sheet)
  expect_identical(selected$Age, c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6"))
})

test_that("the interactive input helper fails clearly outside an interactive session", {
  expect_error(
    StablePopulation:::choose_reconstruction_input_file(is_interactive = FALSE),
    "must be supplied when run_reconstruction_excel\\(\\) is used non-interactively"
  )
})

test_that("run_reconstruction_excel recognizes decorated legacy headers and fixed beta input", {
  input_file <- tempfile(fileext = ".xlsx")
  output_file <- tempfile(fileext = ".xlsx")

  workbook <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(workbook, "legacy_select")
  openxlsx::writeData(
    workbook,
    "legacy_select",
    data.frame(
      "Age (years)" = 0:5,
      "mx (Fertility Rate)" = c(0, 0, 0.30, 0.75, 0.60, 0.20),
      "lx (Survivorship)" = c(1, 0.93, 0.82, 0.67, 0.41, 0.15),
      "Beta" = c(0.70, rep(NA_real_, 5L)),
      check.names = FALSE
    )
  )

  openxlsx::addWorksheet(workbook, "legacy_fixed")
  openxlsx::writeData(
    workbook,
    "legacy_fixed",
    data.frame(
      "Age (years)" = 0:5,
      "mx (Fertility Rate)" = c(0, 0, 0.30, 0.75, 0.60, 0.20),
      "Beta" = c(0.70, rep(NA_real_, 5L)),
      check.names = FALSE
    )
  )

  openxlsx::saveWorkbook(workbook, input_file, overwrite = TRUE)

  result <- run_reconstruction_excel(
    input_file = input_file,
    output_file = output_file,
    beta_values = c(0.5, 1)
  )

  selected <- result$metadata[result$metadata$sheet == "legacy_select", ]
  fixed <- result$metadata[result$metadata$sheet == "legacy_fixed", ]

  expect_identical(selected$route, "select")
  expect_identical(selected$age_column, "Age (years)")
  expect_identical(selected$fertility_column, "mx (Fertility Rate)")
  expect_identical(selected$survivorship_column, "lx (Survivorship)")
  expect_identical(selected$beta_column, "Beta")
  expect_match(selected$note, "fixed beta input was not used")

  expect_identical(fixed$route, "fixed")
  expect_identical(fixed$age_column, "Age (years)")
  expect_identical(fixed$fertility_column, "mx (Fertility Rate)")
  expect_identical(fixed$beta_column, "Beta")
  expect_equal(fixed$fixed_beta, 0.70)
  expect_identical(fixed$n_beta, 1L)

  fixed_output <- readxl::read_excel(output_file, sheet = fixed$result_sheet)
  expect_equal(fixed_output[["Reconstructed survivorship (lx)"]][1L], 1)
  expect_true(all(c(
    "Stable population proportion (R)",
    "Mortality profile (D)",
    "Relative mortality proportion (D_relative)",
    "Conditional survival (B)"
  ) %in% names(fixed_output)))
})
