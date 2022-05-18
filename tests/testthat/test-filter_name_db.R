
# Load function to test
source("R/filter_name_db.R")

testthat::test_that("Name filter function for similar names", {

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = "DALP")

  # Load df1
  test_run <- con %>%
    dplyr::tbl(from = dbplyr::in_schema("ADNSH", "TEST_FILTER_NAME_INPUT"))

  # Load df2
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema("ADNSH", "TEST_FILTER_NAME_EXPECTED"))

  # Process df1
  test_run <- test_run %>%
    filter_name_db(., NAME_A, NAME_B) %>%
    collect()

  # Process df2
  expected_results <- expected_results %>%
    collect()

  # Print to double-check
  print(test_run)
  print(expected_results)

  # Disconnnect
  DBI::dbDisconnect(con)

  # Cheeck if equal
  testthat::expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
