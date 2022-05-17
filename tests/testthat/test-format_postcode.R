test_that("Postcode string formatting", {
  test_run <- format_postcode(readRDS("./testdata/test_postcode_input.rds"), ID, POSTCODE)
  expected_results <- readRDS("./testdata/test_postcode_expected.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
