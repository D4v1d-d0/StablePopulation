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
