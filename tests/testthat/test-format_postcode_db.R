
# Load function to test
source("R/format_postcode_db.R")

testthat::test_that("Postcode string formatting", {

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = "DALP")

  # Load df1
  test_run <- con %>%
    dplyr::tbl(from = dbplyr::in_schema("ADNSH", "TEST_POSTCODE_INPUT"))

  # Load df2
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema("ADNSH", "TEST_POSTCODE_EXPECTED"))

  # Process df1
  test_run <- test_run %>%
    format_postcode_db(., POSTCODE) %>%
    collect()

  # Process df2
  expected_results <- expected_results %>%
    collect()

  # Disconnnect
  DBI::dbDisconnect(con)

  # Cheeck if equal
  testthat::expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
