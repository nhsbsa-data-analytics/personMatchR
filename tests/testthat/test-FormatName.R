test_that("String converts to upper case", {
  expect_equal(FormatName("abcdefghijklmnopqrstuvwxyz"), "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
})

test_that("Numerics are removed", {
  expect_equal(FormatName("a1b2c3d4e5f6g7h8i9j0"), "ABCDEFGHIJ")
})

test_that("Special characters are removed", {
  expect_equal(FormatName("a'b-c d*e+f&g(h)i.j_k,"), "ABCDEFGHIJK")
})

test_that("Null values are skipped and returned as null", {
  expect_equal(FormatName(NA), NA)
})

test_that("Only non alphabetic characters supplied", {
  expect_equal(FormatName("123"), "")
})
