
# Load function to test
source("R/filter_dob_db.R")

testthat::test_that("DOB filter function for similar dates", {

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = "DALP")

  # Load df1
  test_run <- con %>%
    dplyr::tbl(from = dbplyr::in_schema("ADNSH", "TEST_FILTER_DOB_INPUT"))

  # Load df2
  expected_results <- con %>%
    dplyr::tbl(from = dbplyr::in_schema("ADNSH", "TEST_FILTER_DOB_EXPECTED"))

  # Process df1
  test_run <- test_run %>%
    filter_dob_db(., DOB_A, DOB_B, 0.75) %>%
    collect()

  # Process df2
  expected_results <- expected_results %>%
    collect() %>%
    mutate(DOB_SCORE = round((8 - DIFF_DOB) / 8, 2)) %>%
    select(-DIFF_DOB)

  # Print to double-check
  print(test_run)
  print(expected_results)

  # Disconnnect
  DBI::dbDisconnect(con)

  # Cheeck if equal
  testthat::expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
