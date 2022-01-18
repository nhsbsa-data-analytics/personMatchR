library(dplyr)


test_that("Sample test dataset output", {

  #test_run <- FindMatches(readRDS("./tests/test_dataset_a.rds"), readRDS("./tests/test_dataset_b.rds"))
  test_run <- FindMatches(readRDS("../test_dataset_a.rds"), readRDS("../test_dataset_b.rds"))
  #expected_results <- readRDS("./tests/test_match_results.rds")
  expected_results <- readRDS("../test_match_results.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
