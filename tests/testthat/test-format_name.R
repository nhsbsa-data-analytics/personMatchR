test_that("String converts to upper case", {
  expect_equal(format_name("abcdefghijklmnopqrstuvwxyz"), "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
})

test_that("Numerics are removed", {
  expect_equal(format_name("a1b2c3d4e5f6g7h8i9j0"), "ABCDEFGHIJ")
})

test_that("Special characters are removed", {
  expect_equal(format_name("a'b-c d*e+f&g(h)i.j_k,"), "ABCDEFGHIJK")
})

test_that("NA values are skipped and returned as null", {
  expect_equal(format_name(NA), NA)
})

test_that("NULL values are skipped and returned as null", {
  expect_equal(format_name(NULL), NA)
})

test_that("Blank values are skipped and returned as null", {
  expect_equal(format_name(""), NA)
})

test_that("Only non alphabetic characters supplied", {
  expect_equal(format_name("123"), "")
})
