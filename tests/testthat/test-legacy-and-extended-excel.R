test_that("run_analysis retains the StablePopulation 1.0.3 interface", {
  expect_null(formals(run_analysis))
})

test_that("run_reconstruction_excel writes scan and select workbooks", {
  input_file <- tempfile(fileext = ".xlsx")
  output_scan <- tempfile(fileext = ".xlsx")
  output_select <- tempfile(fileext = ".xlsx")

  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "scan_case")
  openxlsx::writeData(
    workbook,
    "scan_case",
    data.frame(
      mx = c(0, 0, 0.30, 0.75, 0.60, 0.20)
    )
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

  scan_result <- run_reconstruction_excel(
    input_file = input_file,
    output_file = output_scan,
    mode = "scan",
    sheets = "scan_case",
    beta_values = c(0.5, 1)
  )
  expect_true(file.exists(output_scan))
  expect_identical(scan_result$metadata$route, "scan")

  select_result <- run_reconstruction_excel(
    input_file = input_file,
    output_file = output_select,
    mode = "auto",
    sheets = "select_case",
    beta_values = c(0.5, 1)
  )
  expect_true(file.exists(output_select))
  expect_identical(select_result$metadata$route, "select")
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

  selected <- readxl::read_excel(expected_output, sheet = processed$profile_sheet)
  expect_identical(selected$age, c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6"))
})
