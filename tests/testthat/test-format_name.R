test_that("Name string formatting", {
  load("./testdata/test_name_input.rda")
  test_run <- format_name(test_name_input, NAME)
  load("./testdata/test_name_expected.rda")
  expected_results <- test_name_expected

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
