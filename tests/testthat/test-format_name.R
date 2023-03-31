test_that("Name string formatting", {
  load("./testdata/test_name_input.rda")
  test_run <- format_name(test_name_input, NAME)
  load("./testdata/test_name_expected.rda")
  expected_results <- test_name_expected

  expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})
