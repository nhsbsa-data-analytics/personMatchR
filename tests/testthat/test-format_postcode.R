test_that("Postcode string formatting", {
  load("./testdata/test_postcode_input.rda")
  test_run <- format_postcode(test_postcode_input, ID, POSTCODE)
  load("./testdata/test_postcode_expected.rda")
  expected_results <- test_postcode_expected

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
