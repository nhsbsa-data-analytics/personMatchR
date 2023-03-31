test_that("Postcode string formatting", {
  load("./testdata/test_postcode_input.rda")
  test_run <- format_postcode(test_postcode_input, ID, POSTCODE)
  load("./testdata/test_postcode_expected.rda")
  expected_results <- test_postcode_expected

  expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})
