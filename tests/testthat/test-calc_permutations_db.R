
testthat::test_that("Match permutation string formatting", {

  # Check if db testing is to be included
  skip_db_tests()

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = db_connection)

  # Load df1
  test_run <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_TEST_CALC_PERMUTATIONS_INPUT"))

  # Load df2
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_TEST_CALC_PERMUTATIONS_EXPECTED"))

  # Process df1
  test_run <- test_run %>%
    calc_permutations_db(., FORENAME, SURNAME, POSTCODE, DOB) %>%
    collect()

  # Process df2
  expected_results <- expected_results %>%
    collect()

  # Disconnnect
  DBI::dbDisconnect(con)

  # Check if equal
  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})
