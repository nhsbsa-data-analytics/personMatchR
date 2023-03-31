test_that("MATCH TEST01: Single exact match (key fields only)", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_inc_exact_match.rda")
  input_b <- match_test_input_b_inc_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  test_run <- calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
    input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    output_type = "key",
    format_data = TRUE,
    inc_no_match = TRUE
  )
  load("./testdata/match_test_output_test01.rda")
  expected_results <- match_test_output_test01

  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})


test_that("MATCH TEST02: multiple confident matches (all fields)", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  test_run <- calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
    input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    output_type = "all",
    format_data = TRUE,
    inc_no_match = TRUE
  )

  # order outputs and apply consistent formatting
  test_run <- test_run %>%
    dplyr::arrange(DF1_INPUT_ID, DF2_INPUT_ID) %>%
    dplyr::mutate(MATCH_COUNT = as.integer(MATCH_COUNT)) %>%
    as.data.frame()
  # round score values
  test_run <- test_run %>%
    dplyr::mutate(
      MATCH_SCORE = round(MATCH_SCORE, 4),
      FORENAME_SCORE = round(FORENAME_SCORE, 4),
      SURNAME_SCORE = round(SURNAME_SCORE, 3),
      DOB_SCORE = round(DOB_SCORE, 2),
      POSTCODE_SCORE = round(POSTCODE_SCORE, 4)
    )

  load("./testdata/match_test_output_test02.rda")
  expected_results <- match_test_output_test02

  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})


test_that("MATCH TEST03: single exact match, plus no match (match fields)", {
  load("./testdata/match_test_input_a_multiple.rda")
  input_a <- match_test_input_a_multiple
  load("./testdata/match_test_input_b_inc_exact_match.rda")
  input_b <- match_test_input_b_inc_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  test_run <- calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
    input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    output_type = "match",
    format_data = TRUE,
    inc_no_match = TRUE
  )

  # order outputs and apply consistent formatting
  test_run <- test_run %>%
    dplyr::arrange(DF1_INPUT_ID, DF2_INPUT_ID) %>%
    dplyr::mutate(MATCH_COUNT = as.integer(MATCH_COUNT)) %>%
    as.data.frame()

  load("./testdata/match_test_output_test03.rda")
  expected_results <- match_test_output_test03

  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})



test_that("MATCH TEST04: single no match (key fields)", {
  load("./testdata/match_test_input_a_no_match.rda")
  input_a <- match_test_input_a_no_match
  load("./testdata/match_test_input_b_inc_exact_match.rda")
  input_b <- match_test_input_b_inc_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  test_run <- calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
    input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    output_type = "key",
    format_data = TRUE,
    inc_no_match = TRUE
  )

  # order outputs and apply consistent formatting
  test_run <- test_run %>%
    dplyr::arrange(DF1_INPUT_ID, DF2_INPUT_ID) %>%
    dplyr::mutate(MATCH_COUNT = as.integer(MATCH_COUNT)) %>%
    as.data.frame()

  load("./testdata/match_test_output_test04.rda")
  expected_results <- match_test_output_test04

  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})


test_that("MATCH TEST05: date formats - multiple exact matches (key fields)", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_date_format_mix.rda")
  input_b <- match_test_input_b_date_format_mix %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  test_run <- calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
    input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    output_type = "key",
    format_data = TRUE,
    inc_no_match = TRUE
  )

  # order outputs and apply consistent formatting
  test_run <- test_run %>%
    dplyr::arrange(DF1_INPUT_ID, DF2_INPUT_ID) %>%
    dplyr::mutate(MATCH_COUNT = as.integer(MATCH_COUNT)) %>%
    as.data.frame()

  load("./testdata/match_test_output_test05.rda")
  expected_results <- match_test_output_test05

  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})


test_that("MATCH TEST06: postcode formats - multiple exact matches (key fields)", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_postcode_format_mix.rda")
  input_b <- match_test_input_b_postcode_format_mix %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  test_run <- calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
    input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    output_type = "key",
    format_data = TRUE,
    inc_no_match = TRUE
  )

  # order outputs and apply consistent formatting
  test_run <- test_run %>%
    dplyr::arrange(DF1_INPUT_ID, DF2_INPUT_ID) %>%
    dplyr::mutate(MATCH_COUNT = as.integer(MATCH_COUNT)) %>%
    as.data.frame()

  load("./testdata/match_test_output_test06.rda")
  expected_results <- match_test_output_test06

  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})


# tests for match weighting arguments
test_that("MW01: Match weighting error message thrown when supplied weightings do not total 100%", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_forename = 0.9
    ),
    "Supplied score weighting values do not total 100%"
  )
})

test_that("MW02: Match weighting error message thrown when supplied weightings passed as non numeric value - forename weighting", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_forename = "0.9"
    ),
    "Non numeric value supplied as weighting factor"
  )
})

test_that("MW03: Match weighting error message thrown when supplied weightings passed as non numeric value - surname weighting", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_surname = "0.9"
    ),
    "Non numeric value supplied as weighting factor"
  )
})

test_that("MW04: Match weighting error message thrown when supplied weightings passed as non numeric value - dob weighting", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_dob = "0.9"
    ),
    "Non numeric value supplied as weighting factor"
  )
})

test_that("MW05: Match weighting error message thrown when supplied weightings passed as non numeric value - postcode weighting", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_postcode = "0.9"
    ),
    "Non numeric value supplied as weighting factor"
  )
})

test_that("MW06: Match weighting error message thrown when invalid weightings passed - forename weighting < 0", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_forename = -0.1
    ),
    "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW07: Match weighting error message thrown when invalid weightings passed - forename weighting > 1", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_forename = 1.1
    ),
    "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW08: Match weighting error message thrown when invalid weightings passed - surname weighting < 0", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_surname = -0.1
    ),
    "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW09: Match weighting error message thrown when invalid weightings passed - surname weighting > 1", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_surname = 1.1
    ),
    "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW10: Match weighting error message thrown when invalid weightings passed - dob weighting < 0", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_dob = -0.1
    ),
    "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW11: Match weighting error message thrown when invalid weightings passed - dob weighting > 1", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_dob = 1.1
    ),
    "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW12: Match weighting error message thrown when invalid weightings passed - postcode weighting < 0", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_postcode = -0.1
    ),
    "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW13: Match weighting error message thrown when invalid weightings passed - postcode weighting > 1", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_postcode = 1.1
    ),
    "Individual field score weighting values must be between 0.0 and 1.0"
  )
})

test_that("MW14: Match weighting error message thrown when invalid weightings passed - combined weighting > 1", {
  load("./testdata/match_test_input_a_single.rda")
  input_a <- match_test_input_a_single
  load("./testdata/match_test_input_b_excl_exact_match.rda")
  input_b <- match_test_input_b_excl_exact_match %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )
  testthat::expect_error(
    calc_match_person(input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
      input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
      output_type = "all",
      format_data = TRUE,
      inc_no_match = TRUE,
      sw_forename = 0.3,
      sw_surname = 0.3,
      sw_dob = 0.3,
      sw_postcode = 0.3
    ),
    "Supplied score weighting values do not total 100%"
  )
})
