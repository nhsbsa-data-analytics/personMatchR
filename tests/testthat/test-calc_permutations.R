test_that("Match permutation string formatting", {
  load("./testdata/test_calc_permutations_input.rda")
  test_run <- calc_permutations(test_calc_permutations_input, FORENAME, SURNAME, POSTCODE, DOB)
  load("./testdata/test_calc_permutations_expected.rda")
  expected_results <- test_calc_permutations_expected

  expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})
