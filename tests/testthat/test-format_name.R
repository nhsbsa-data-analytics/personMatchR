test_that("Name string formatting", {
  test_run <- format_name(readRDS("./testdata/test_name_input.rds"), NAME)
  expected_results <- readRDS("./testdata/test_name_expected.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
