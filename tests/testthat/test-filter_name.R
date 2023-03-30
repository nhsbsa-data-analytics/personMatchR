test_that("Name filter function for similar names", {
  load("./testdata/test_filter_name_input.rda")
  test_run <- filter_name(test_filter_name_input, NAME_A, NAME_B)
  load("./testdata/test_filter_name_expected.rda")
  expected_results <- test_filter_name_expected

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
