test_that("Name filter function for similar names", {
  test_run <- filter_name(readRDS("./testdata/test_filter_name_input.rds"), NAME_A, NAME_B)
  expected_results <- readRDS("./testdata/test_filter_name_expected.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
