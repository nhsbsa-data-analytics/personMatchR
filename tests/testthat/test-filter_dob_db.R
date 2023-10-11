
testthat::test_that("DOB filter function for similar dates", {

  # Check if db testing is to be included
  skip_db_tests()

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = db_connection)

  # Load df1
  test_run <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_TEST_FILTER_DOB_INPUT"))

  # Load df2
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_TEST_FILTER_DOB_EXPECTED"))

  # Process df1
  test_run <- test_run %>%
    filter_dob_db(., DOB_A, DOB_B, 0.75) %>%
    dplyr::collect()

  # Process df2
  expected_results <- expected_results %>%
    dplyr::collect()

  # Disconnnect
  DBI::dbDisconnect(con)

  # Cheeck if equal
  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})
