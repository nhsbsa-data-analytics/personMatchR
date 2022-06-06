test_that("DOB filter function for similar dates", {
  test_run <- filter_dob(readRDS("./testdata/test_filter_dob_input.rds"), DOB_A, DOB_B, 0.75)
  expected_results <- readRDS("./testdata/test_filter_dob_expected.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
