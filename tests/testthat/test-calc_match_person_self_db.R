test_that("SMDB01: Self match output with key fields and all matches", {

  # Check if db testing is to be included
  skip_db_tests()

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = db_connection)

  # Load input dataset
  input <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_INPUT_SELF_MATCH"))

  # Run matching process
  test_run <- calc_match_person_self_db(
    input, ID, FORENAME, SURNAME, DOB, POSTCODE,
    output_type = "key",
    inc_no_match = TRUE,
    unique_combinations_only = FALSE
  )  |>
    dplyr::mutate(MATCH_SCORE = round(MATCH_SCORE,4)) |>
    dplyr::collect()

  # Load expected results
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_OUTPUT_SELF_MATCH_ALL_KEY")) %>%
    dplyr::collect()

  # Disconnect
  DBI::dbDisconnect(con)

  # Check if equal
  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)

})

test_that("SMDB02: Self match output with key fields and unique combinations of matches only", {


  # Check if db testing is to be included
  skip_db_tests()

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = db_connection)

  # Load input dataset
  input <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_INPUT_SELF_MATCH"))

  # Run matching process
  test_run <- calc_match_person_self_db(
    input, ID, FORENAME, SURNAME, DOB, POSTCODE,
    output_type = "key",
    inc_no_match = TRUE,
    unique_combinations_only = TRUE
  )  |>
    dplyr::mutate(MATCH_SCORE = round(MATCH_SCORE,4)) |>
    dplyr::collect()

  # Load expected results
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_MATCH_TEST_OUTPUT_SELF_MATCH_UNIQUE_COMBOS_KEY")) %>%
    dplyr::collect()

  # Disconnect
  DBI::dbDisconnect(con)

  # Check if equal
  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)

})
