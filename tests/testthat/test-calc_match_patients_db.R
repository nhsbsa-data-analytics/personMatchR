
# Test One ---------------------------------------------------------------------
testthat::test_that("MATCH TEST01: Single exact match (key fields only)", {

  # Check if db testing is to be included
  skip_db_tests()

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = db_connection)

  # Load df1
  input_a <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_INPUT_A_SINGLE"))

  # Load df2
  input_b <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_INPUT_B_INC_EXACT_MATCH"))

  # Process df2
  input_b <- input_b %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )

  # Process df1
  test_run <- calc_match_patients_db(
    input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
    input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    output_type = "key",
    format_data = FALSE,
    inc_no_match = TRUE
  ) %>%
  collect()

  # Expected Results
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_OUTPUT_TEST01")) %>%
    collect()

  # Disconnect
  DBI::dbDisconnect(con)

  # Check if equal
  testthat::expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})

# Test Two ---------------------------------------------------------------------
testthat::test_that("MATCH TEST02: multiple confident matches (all fields)", {

  # Check if db testing is to be included
  skip_db_tests()

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = db_connection)

  # Load df1
  input_a <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_INPUT_A_SINGLE"))

  # Load df2
  input_b <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_INPUT_B_EXCL_EXACT_MATCH"))

  # Process df2
  input_b <- input_b %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )

  # Process df1
  test_run <- calc_match_patients_db(
    input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
    input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    output_type = "all",
    format_data = FALSE,
    inc_no_match = TRUE
    ) %>%
    collect()

  # order outputs and apply consistent formatting
  test_run <- test_run %>%
    dplyr::arrange(ID, ID_TWO) %>%
    as.data.frame()

  # round score values
  test_run <- test_run %>%
    mutate(
      MATCH_SCORE = round(MATCH_SCORE, 4),
      FORENAME_SCORE = round(FORENAME_SCORE, 4),
      SURNAME_SCORE = round(SURNAME_SCORE, 4),
      DOB_SCORE = round(DOB_SCORE, 4),
      POSTCODE_SCORE = round(POSTCODE_SCORE, 4)
  )

  # Expected Results
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_OUTPUT_TEST02")) %>%
    collect()

  # Process expected results
  expected_results <- expected_results %>%
    mutate(
      MATCH_SCORE = round(MATCH_SCORE, 4),
      FORENAME_SCORE = round(FORENAME_SCORE, 4),
      SURNAME_SCORE = round(SURNAME_SCORE, 4),
      DOB_SCORE = round(DOB_SCORE, 4),
      POSTCODE_SCORE = round(POSTCODE_SCORE, 4)
    )

  # Disconnect
  DBI::dbDisconnect(con)

  # Check if equal
  testthat::expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})

# Test Three -------------------------------------------------------------------
testthat::test_that("MATCH TEST03: single exact match, plus no match (match fields)", {

  # Check if db testing is to be included
  skip_db_tests()

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = db_connection)

  # Load df1
  input_a <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_INPUT_A_MULTIPLE"))

  # Load df2
  input_b <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_INPUT_B_INC_EXACT_MATCH"))

  # Process df2
  input_b <- input_b %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )

  # Process df1
  test_run <- calc_match_patients_db(
    input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
    input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    output_type = "match",
    format_data = FALSE,
    inc_no_match = TRUE
    ) %>%
    collect()

  # order outputs and apply consistent formatting
  test_run <- test_run %>%
    dplyr::arrange(ID, ID_TWO) %>%
    as.data.frame()

  # Expected Results
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_OUTPUT_TEST03")) %>%
    collect()

  # Disconnect
  DBI::dbDisconnect(con)

  # Check if equal
  testthat::expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})

# Test Four --------------------------------------------------------------------
testthat::test_that("MATCH TEST04: single no match (key fields)", {

  # Check if db testing is to be included
  skip_db_tests()

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = db_connection)

  # Load df1
  input_a <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_INPUT_A_NO_MATCH"))

  # Load df2
  input_b <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_INPUT_B_INC_EXACT_MATCH"))

  # Process df2
  input_b <- input_b %>%
    dplyr::rename(
      ID_TWO = ID,
      FORENAME_TWO = FORENAME,
      SURNAME_TWO = SURNAME,
      DOB_TWO = DOB,
      POSTCODE_TWO = POSTCODE
    )

  # Process df1
  test_run <- calc_match_patients_db(
    input_a, ID, FORENAME, SURNAME, DOB, POSTCODE,
    input_b, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    output_type = "key",
    format_data = FALSE,
    inc_no_match = TRUE
  ) %>%
    collect()

  # order outputs and apply consistent formatting
  test_run <- test_run %>%
    dplyr::arrange(ID, ID_TWO) %>%
    as.data.frame()

  # Expected Results
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_OUTPUT_TEST04")) %>%
    dplyr::mutate(ID_TWO = as.character(ID_TWO)) %>%
    collect()

  # Disconnnect
  DBI::dbDisconnect(con)

  # Check if equal
  testthat::expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
