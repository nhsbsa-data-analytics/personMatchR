test_that("Null values are skipped and returned as null", {
  expect_equal(format_postcode(NA), NA)
})