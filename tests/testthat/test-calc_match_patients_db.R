
# Load function to test
source("R/calc_match_patients_db.R")
source("R/calc_permutations_db.R")
source("R/filter_dob_db.R")
source("R/filter_name_db.R")
source("R/format_date_db.R")
source("R/format_name_db.R")
source("R/format_postcode_db.R")

testthat::test_that("MATCH TEST01: Single exact match (key fields only", {

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = "DALP")

  # Load df1
  input_a <- con %>%
    dplyr::tbl(from = dbplyr::in_schema("ADNSH", "MATCH_TEST_INPUT_A_SINGLE"))

  # Load df2
  input_b <- con %>%
    dplyr::tbl(from = dbplyr::in_schema("ADNSH", "MATCH_TEST_INPUT_B_INC_EXACT_MATCH"))

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
    dplyr::tbl(from = dbplyr::in_schema("ADNSH", "MATCH_TEST_OUTPUT_TEST01")) %>%
    collect()

  # Print to double-check
  print(test_run)
  print(expected_results)

  # Disconnnect
  DBI::dbDisconnect(con)

  # Cheeck if equal
  testthat::expect_equal(dplyr::all_equal(test_run, expected_results), TRUE)
})
