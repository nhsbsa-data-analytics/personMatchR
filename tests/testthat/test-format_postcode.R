test_that("NA values are skipped and returned as null", {
  expect_equal(format_postcode(NA), NA)
})


test_that("NULL values are skipped and returned as null", {
  expect_equal(format_postcode(NULL), NA)
})


test_that("Blank string values are skipped and returned as null", {
  expect_equal(format_postcode(""), NA)
})
