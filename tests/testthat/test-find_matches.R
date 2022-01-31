test_that("Sample test dataset output - exact match", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"))
  expected_results <- readRDS("./testdata/test_match_results_b.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})

test_that("Sample test dataset output - confident matches", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_c.rds"))
  expected_results <- readRDS("./testdata/test_match_results_c.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
