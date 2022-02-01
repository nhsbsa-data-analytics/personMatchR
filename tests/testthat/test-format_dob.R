test_that("NA values are skipped and returned as null", {
  expect_equal(format_dob(NA), NA)
})

test_that("NULL values are skipped and returned as null", {
  expect_equal(format_dob(NULL), NA)
})

test_that("Date values return the same date value", {
  # define test dates
  test_value <- as.Date("25-12-2021","%d-%m-%Y")
  expected_results <- "20211225"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})
