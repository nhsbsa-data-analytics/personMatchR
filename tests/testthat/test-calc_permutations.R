test_that("Match permutation string formatting", {
  test_run <- calc_permutations(readRDS("./testdata/test_calc_permutations_input.rds"), FORENAME, SURNAME, POSTCODE, DOB)
  expected_results <- readRDS("./testdata/test_calc_permutations_expected.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
