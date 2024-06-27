
testthat::test_that("Name filter function for similar names", {

  # Check if db testing is to be included
  skip_db_tests()

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = db_connection)

  # Load df1
  test_run <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_TEST_FILTER_NAME_INPUT"))

  # Load df2
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema(db_cypher, "PERSONMATCHR_TEST_FILTER_NAME_EXPECTED"))

  # Process df1
  test_run <- test_run %>%
    filter_name_db(., NAME_A, NAME_B) %>%
    dplyr::collect()

  # Process df2
  expected_results <- expected_results %>%
    dplyr::collect()

  # Disconnnect
  DBI::dbDisconnect(con)

  # Cheeck if equal
  testthat::expect_equal(all.equal(as.data.frame(test_run), as.data.frame(expected_results)), TRUE)
})
