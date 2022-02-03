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

# tests for inclusion of no match results
test_that("NM01: Sample test dataset output - exact match - exclude no matches", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), inc_no_match = FALSE)
  expected_results <- readRDS("./testdata/test_match_results_b.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})

test_that("NM02: Sample test dataset output - confident matches - exclude no matches", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_c.rds"), inc_no_match = FALSE)
  expected_results <- readRDS("./testdata/test_match_results_c.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})


test_that("NM03: Sample test dataset output - exact match - include no matches", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), inc_no_match = TRUE)
  expected_results <- readRDS("./testdata/test_match_results_NM03.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})


test_that("NM04: Sample test dataset output - confident matches - include no matches", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_c.rds"), inc_no_match = TRUE)
  expected_results <- readRDS("./testdata/test_match_results_NM04.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})

# tests for match weighting arguments
test_that("MW01: Match weighting error message thrown when supplied weightings do not total 100%", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_forename = 0.9),
               "Supplied score weighting values do not total 100%"
               )
})

test_that("MW02: Match weighting error message thrown when supplied weightings passed as non numeric value - forename weighting", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_forename = "0.9"),
               "Non numeric value supplied as weighting factor"
  )
})

test_that("MW03: Match weighting error message thrown when supplied weightings passed as non numeric value - surname weighting", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_surname = "0.9"),
               "Non numeric value supplied as weighting factor"
  )
})

test_that("MW04: Match weighting error message thrown when supplied weightings passed as non numeric value - dob weighting", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_dob = "0.9"),
               "Non numeric value supplied as weighting factor"
  )
})

test_that("MW05: Match weighting error message thrown when supplied weightings passed as non numeric value - postcode weighting", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_postcode = "0.9"),
               "Non numeric value supplied as weighting factor"
  )
})

test_that("MW06: Match weighting error message thrown when invalid weightings passed - forename weighting < 0", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_forename = -0.1),
               "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW07: Match weighting error message thrown when invalid weightings passed - forename weighting > 1", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_forename = 1.1),
               "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW08: Match weighting error message thrown when invalid weightings passed - surname weighting < 0", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_surname = -0.1),
               "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW09: Match weighting error message thrown when invalid weightings passed - surname weighting > 1", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_surname = 1.1),
               "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW10: Match weighting error message thrown when invalid weightings passed - dob weighting < 0", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_dob = -0.1),
               "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW11: Match weighting error message thrown when invalid weightings passed - dob weighting > 1", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_dob = 1.1),
               "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW12: Match weighting error message thrown when invalid weightings passed - postcode weighting < 0", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_postcode = -0.1),
               "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW13: Match weighting error message thrown when invalid weightings passed - postcode weighting > 1", {

  expect_error(find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_b.rds"), sw_postcode = 1.1),
               "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW14: Match weighting defined weightings applied", {

  test_run <- find_matches(readRDS("./testdata/test_dataset_a.rds"), readRDS("./testdata/test_dataset_c.rds"), inc_no_match = TRUE, sw_forename = 0.25, sw_surname = 0.25, sw_dob = 0.25,sw_postcode = 0.25)
  expected_results <- readRDS("./testdata/test_match_results_MW14.rds")

  expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
