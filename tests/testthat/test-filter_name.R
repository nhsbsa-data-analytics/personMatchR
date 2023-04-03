test_that("Name filter function for similar names", {
  load("./testdata/test_filter_name_input.rda")
  test_run <- filter_name(test_filter_name_input, NAME_A, NAME_B)
  load("./testdata/test_filter_name_expected.rda")
  expected_results <- test_filter_name_expected

  expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})
