#' Test cases for calc_permutations function
#' Test dataset creation
#'
#' BUild the dataframes for the test cases
#'


# Test Case Dataset ---------------------------------------------------------------------------
# create initial test case
TEST_INPUT <- data.frame(
  ID = 1,
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  POSTCODE = "NE15 8NY",
  DOB = "19720824"
)

# create expect output
TEST_OUTPUT <- data.frame(
  ID = 1,
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  POSTCODE = "NE15 8NY",
  DOB = "19720824"
) %>%
  dplyr::mutate(
    PERM1 = paste0(FORENAME, SURNAME, DOB), # forename, surname and dob
    PERM2 = paste0(FORENAME, POSTCODE, DOB), # forname, postcode and dob
    PERM3 = paste0(SURNAME, POSTCODE, DOB), # surname, postcode and dob
    PERM4 = paste0(
      substr(FORENAME, 1, 1), # first character of forename
      substr(SURNAME, 1, 4), # first 4 characters of surname
      substr(POSTCODE, 1, 4) # first 4 characters of postcode
    ),
    PERM5 = paste0(
      substr(FORENAME, 1, 3), # first 3 characters of forename
      substr(SURNAME, 1, 2), # first 2 characters of surname
      substr(POSTCODE, 1, 2) # first 2 characters of postcode
    ),
    PERM6 = paste0(
      substr(FORENAME, nchar(FORENAME) - 2, nchar(FORENAME)), # last 3 characters of forename
      substr(SURNAME, 1, 3), # first 3 characters of surname
      substr(POSTCODE, 1, 3) # first 3 characters of postcode
    ),
    PERM7 = paste0(
      substr(gsub("[AEIOU]", "", FORENAME), 1, 3), # first 3 consonants of forename
      substr(SURNAME, 1, 3), # first 3 characters of surname
      substr(POSTCODE, 1, 3) # first 3 characters of postcode
    ),
    PERM8 = paste0(
      gsub("[AEIOU]", "", FORENAME), # all consonants of forename
      substr(SURNAME, 1, 2), # first 2 characters of surname
      substr(POSTCODE, 1, 2) # first 2 characters of postcode
    ),
    PERM9 = paste0(
      gsub("[B-DF-HJ-NP-TV-Z]", "", FORENAME), # All vowels from forename
      substr(SURNAME, 1, 3), # first 3 characters of surname
      substr(POSTCODE, 1, 3) # first 3 characters of postcode
    )
  )


# Save files to test folder
saveRDS(TEST_INPUT, file = "./tests/testthat/testdata/test_calc_permutations_input.rds")
saveRDS(TEST_OUTPUT, file = "./tests/testthat/testdata/test_calc_permutations_expected.rds")
