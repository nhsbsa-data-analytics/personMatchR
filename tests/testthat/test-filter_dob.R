test_that("DOB filter function for similar dates", {
  load("./testdata/test_filter_dob_input.rda")
  test_run <- filter_dob(test_filter_dob_input, DOB_A, DOB_B, 0.75)
  load("./testdata/test_filter_dob_expected.rda")
  expected_results <- test_filter_dob_expected

  expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})
