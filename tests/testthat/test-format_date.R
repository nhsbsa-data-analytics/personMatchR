test_that("Date string formatting", {
  load("./testdata/test_date_input.rda")
  test_run <- format_date(test_date_input, DOB)
  load("./testdata/test_date_expected.rda")
  expected_results <- test_date_expected

  expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})

test_that("Date integer formatting", {
  load("./testdata/test_date_input_integer.rda")
  test_run <- format_date(test_date_input_integer, DOB)
  load("./testdata/test_date_expected_integer.rda")
  expected_results <- test_date_expected_integer

  expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})

test_that("Date date formatting", {
  load("./testdata/test_date_input_date.rda")
  test_run <- format_date(test_date_input_date, DOB)
  load("./testdata/test_date_expected_date.rda")
  expected_results <- test_date_expected_date

  expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})
