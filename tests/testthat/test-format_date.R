test_that("Date string formatting", {
  load("./testdata/test_date_input.rda")
  test_run <- format_date(test_date_input, DOB)
  load("./testdata/test_date_expected.rda")
  expected_results <- test_date_expected

  expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})
