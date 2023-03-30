test_that("Date string formatting", {
  load("./testdata/test_date_input.rda")
  test_run <- format_date(test_date_input, DOB)
  load("./testdata/test_date_expected.rda")
  expected_results <- test_date_expected

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
