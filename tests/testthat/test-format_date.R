test_that("Date string formatting", {
  test_run <- format_date(readRDS("./testdata/test_date_input.rds"), DOB)
  expected_results <- readRDS("./testdata/test_date_expected.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
