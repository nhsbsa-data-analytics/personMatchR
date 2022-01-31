test_that("Sample test dataset output - exact match", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"))
  expected_results <- readRDS("./testdata/test_match_results_b.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})


test_that("Sample test dataset output - exact match - exclude no matches", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), inc_no_match = FALSE)
  expected_results <- readRDS("./testdata/test_match_results_b.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})


test_that("Sample test dataset output - confident matches", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_c.rds"))
  expected_results <- readRDS("./testdata/test_match_results_c.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})


test_that("Sample test dataset output - confident matches - exclude no matches", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_c.rds"), inc_no_match = FALSE)
  expected_results <- readRDS("./testdata/test_match_results_c.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})


test_that("Sample test dataset output - exact match - include no matches", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), inc_no_match = TRUE)
  expected_results <- readRDS("./testdata/test_match_results_d.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})


test_that("Sample test dataset output - confident matches - include no matches", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_c.rds"), inc_no_match = TRUE)
  expected_results <- readRDS("./testdata/test_match_results_e.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
