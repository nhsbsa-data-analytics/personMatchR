test_that("SM01: Self match output with key fields and all matches", {
  load("./testdata/match_test_input_self_match.rda")
  input_df <- match_test_input_self_match

  test_run <- calc_match_person_self(
    df = input_df,
    id = ID,
    forename = FORENAME,
    surname = SURNAME,
    dob = DOB,
    postcode = POSTCODE,
    output_type = c("key"),
    format_data = c(TRUE),
    inc_no_match = c(TRUE),
    unique_combinations_only = c(FALSE)
  )

  # order outputs and apply consistent formatting
  test_run <- test_run  |>
    dplyr::arrange(DF1_INPUT_ID, DF2_INPUT_ID)  |>
    dplyr::mutate(MATCH_COUNT = as.integer(MATCH_COUNT),
                  MATCH_SCORE = round(MATCH_SCORE, 4)
    )  |>
    as.data.frame()

  load("./testdata/match_test_output_self_match_all_key.rda")
  expected_results <- match_test_output_self_match_all_key

  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)

})

test_that("SM02: Self match output with key fields and unique combinations of matches only", {
  load("./testdata/match_test_input_self_match.rda")
  input_df <- match_test_input_self_match

  test_run <- calc_match_person_self(
    df = input_df,
    id = ID,
    forename = FORENAME,
    surname = SURNAME,
    dob = DOB,
    postcode = POSTCODE,
    output_type = c("key"),
    format_data = c(TRUE),
    inc_no_match = c(TRUE),
    unique_combinations_only = c(TRUE)
  )

  # order outputs and apply consistent formatting
  test_run <- test_run  |>
    dplyr::arrange(DF1_INPUT_ID, DF2_INPUT_ID)  |>
    dplyr::mutate(MATCH_COUNT = as.integer(MATCH_COUNT),
                  MATCH_SCORE = round(MATCH_SCORE, 4)
    )  |>
    as.data.frame()

  load("./testdata/match_test_output_self_match_unique_combos_key.rda")
  expected_results <- match_test_output_self_match_unique_combos_key

  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)

})
