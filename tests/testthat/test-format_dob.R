test_that("NA values are skipped and returned as null", {
  expect_equal(format_dob(NA), NA)
})

test_that("NULL values are skipped and returned as null", {
  expect_equal(format_dob(NULL), NA)
})

test_that("YMD", {
  # define test dates
  test_value <- "2004-03-21"
  expected_results <- "20040321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("YMD_Char", {
  # define test dates
  test_value <- "2004-Mar-21"
  expected_results <- "20040321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("YMD", {
  # define test dates
  test_value <- "2004/03/21"
  expected_results <- "20040321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})


test_that("YDM", {
  # define test dates
  test_value <- "2004/21/03"
  expected_results <- "20040321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("YDM_Char", {
  # define test dates
  test_value <- "2004/21/Mar"
  expected_results <- "20040321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("YMD_Char", {
  # define test dates
  test_value <- "2004/March/21"
  expected_results <- "20040321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("MDY", {
  # define test dates
  test_value <- "03/13/2021"
  expected_results <- "20210313"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})


test_that("MDY_Char", {
  # define test dates
  test_value <- "July/03/2021"
  expected_results <- "20210703"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("DMY", {
  # define test dates
  test_value <- "21-03-2005"
  expected_results <- "20050321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("DMY_2", {
  # define test dates
  test_value <- "21/03/2005"
  expected_results <- "20050321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("DMY_Char", {
  # define test dates
  test_value <- "21-Mar-2005"
  expected_results <- "20050321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("DMY_Char_2", {
  # define test dates
  test_value <- "21-mar-2005"
  expected_results <- "20050321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("DMY_Char_3", {
  # define test dates
  test_value <- "21-March-2005"
  expected_results <- "20050321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})





test_that("DMY", {
  # define test dates
  test_value <- "21/03/2005"
  expected_results <- "20050321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})


test_that("DMY_Char", {
  # define test dates
  test_value <- "21/Mar/2005"
  expected_results <- "20050321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("DMY_Char", {
  # define test dates
  test_value <- "21/march/2005"
  expected_results <- "20050321"
  # run function
  expect_equal(format_dob(test_value), expected_results)
})

test_that("Error_Date", {
  # define test dates
  test_value <- "41/march/2005"
  expected_results <- NA
  # run function
  expect_equal(format_dob(test_value), expected_results)
})
