#' Test cases for calc_match_patients function
#' Test dataset creation
#'
#' BUild the dataframes for the test cases
#'


# INPUT_A_SINGLE_CASE ---------------------------------------------------------------------------
# file for "left hand side" of testing
# this test dataset will only include a single test case
# a single record may identify possible issues with the process after an exact match is found
# if this is exactly matched there will be nothing to pass to the rest of the tests

INPUT_A_SINGLE_CASE <- data.frame(
  ID = "1",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24"
)

saveRDS(INPUT_A_SINGLE_CASE, file = "./tests/testthat/testdata/match_test_input_a_single.rds")



# INPUT_A_MULTIPLE_CASE -------------------------------------------------------------------------
# file for "left hand side" of testing
# adds an additional test case that will not appear in any of other test cases so will alway "no match"

INPUT_A_MULTIPLE_CASE <- rbind(INPUT_A_SINGLE_CASE, data.frame(
  ID = "2",
  SURNAME = "BRASI",
  FORENAME = "LUCA",
  POSTCODE = "NE6 1JS",
  DOB = "1972-01-01"
))

saveRDS(INPUT_A_MULTIPLE_CASE, file = "./tests/testthat/testdata/match_test_input_a_multiple.rds")



# INPUT_A_NO_MATCH_CASE -------------------------------------------------------------------------
# file for "left hand side" of testing
# case where there is only a no match

INPUT_A_NO_MATCH_CASE <- data.frame(
  ID = "2",
  SURNAME = "BRASI",
  FORENAME = "LUCA",
  POSTCODE = "NE6 1JS",
  DOB = "1972-01-01"
)

saveRDS(INPUT_A_NO_MATCH_CASE, file = "./tests/testthat/testdata/match_test_input_a_no_match.rds")



# INPUT_B_INC_EXACT_MATCH ---------------------------------------------------------------------
# file for "right hand side" of testing
# will include multiple potential matches
# including one which is an exact match
INPUT_B_INC_EXACT_MATCH <- data.frame(
  ID = "1",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
)
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "2",
  SURNAME = "CORLEON",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Confident - surname close"
))
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "3",
  SURNAME = "CORLEONE",
  FORENAME = "MICHEAL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Confident - forename close"
))
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "4",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE6 1JS",
  DOB = "1972-08-24",
  NOTES = "Confident - postcode close"
))
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "5",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-25",
  NOTES = "Confident - DOB 1 out"
))
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "6",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1973-08-25",
  NOTES = "Confident - DOB 2 out"
))
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "7",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = NA,
  DOB = "1972-08-24",
  NOTES = "Confident - postcode missing"
))
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "8",
  SURNAME = NA,
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Confident - surname missing"
))
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "9",
  SURNAME = "CORLEON",
  FORENAME = "MICHEAL",
  POSTCODE = "NE1 5NY",
  DOB = "1972-08-25",
  NOTES = "Confident - all close"
))
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "10",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1973-09-25",
  NOTES = "No match - DOB out by 3+"
))
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "11",
  SURNAME = "CORLEONE",
  FORENAME = "FREDO",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "No match - forename different"
))
INPUT_B_INC_EXACT_MATCH <- rbind(INPUT_B_INC_EXACT_MATCH, data.frame(
  ID = "12",
  SURNAME = "HAGAN",
  FORENAME = "TOM",
  POSTCODE = "SW1A 1AA",
  DOB = "1991-03-08",
  NOTES = "No match - all different"
))

saveRDS(INPUT_B_INC_EXACT_MATCH, file = "./tests/testthat/testdata/match_test_input_b_inc_exact_match.rds")


# INPUT_B_EXCL_EXACT_MATCH --------------------------------------------------------------------
# file for "right hand side" of testing
# will include multiple potential matches
# none of these will be an exact match though

INPUT_B_EXCL_EXACT_MATCH <- INPUT_B_INC_EXACT_MATCH %>%
  dplyr::filter(ID != "1")

saveRDS(INPUT_B_EXCL_EXACT_MATCH, file = "./tests/testthat/testdata/match_test_input_b_excl_exact_match.rds")



# INPUT_B_DATE_FORMAT_MIX ---------------------------------------------------------------------
# file for "right hand side" of testing
# will include multiple potential matches
# all should be an exact match but only with different formatting of the DOB string
INPUT_B_DATE_FORMAT_MIX <- data.frame(
  ID = "1",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
)
INPUT_B_DATE_FORMAT_MIX <- rbind(INPUT_B_DATE_FORMAT_MIX, data.frame(
  ID = "2",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-24-08",
  NOTES = "Exact match"
))
INPUT_B_DATE_FORMAT_MIX <- rbind(INPUT_B_DATE_FORMAT_MIX, data.frame(
  ID = "3",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "24/08/1972",
  NOTES = "Exact match"
))
INPUT_B_DATE_FORMAT_MIX <- rbind(INPUT_B_DATE_FORMAT_MIX, data.frame(
  ID = "4",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "24-aug-1972",
  NOTES = "Exact match"
))
INPUT_B_DATE_FORMAT_MIX <- rbind(INPUT_B_DATE_FORMAT_MIX, data.frame(
  ID = "5",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "08-24-1972",
  NOTES = "Exact match"
))
INPUT_B_DATE_FORMAT_MIX <- rbind(INPUT_B_DATE_FORMAT_MIX, data.frame(
  ID = "6",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "august 24 1972",
  NOTES = "Exact match"
))

saveRDS(INPUT_B_DATE_FORMAT_MIX, file = "./tests/testthat/testdata/match_test_input_b_date_format_mix.rds")



# INPUT_B_POSTCODE_FORMAT_MIX -----------------------------------------------------------------
# file for "right hand side" of testing
# will include multiple potential matches
# all should be an exact match but only with different formatting of the POSTCODE string

INPUT_B_POSTCODE_FORMAT_MIX <- data.frame(
  ID = "1",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
)
INPUT_B_POSTCODE_FORMAT_MIX <- rbind(INPUT_B_POSTCODE_FORMAT_MIX, data.frame(
  ID = "2",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE158NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
))
INPUT_B_POSTCODE_FORMAT_MIX <- rbind(INPUT_B_POSTCODE_FORMAT_MIX, data.frame(
  ID = "3",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15-8NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
))
INPUT_B_POSTCODE_FORMAT_MIX <- rbind(INPUT_B_POSTCODE_FORMAT_MIX, data.frame(
  ID = "4",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NEI58NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
))
INPUT_B_POSTCODE_FORMAT_MIX <- rbind(INPUT_B_POSTCODE_FORMAT_MIX, data.frame(
  ID = "5",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NEl58NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
))
INPUT_B_POSTCODE_FORMAT_MIX <- rbind(INPUT_B_POSTCODE_FORMAT_MIX, data.frame(
  ID = "6",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE1  58NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
))

saveRDS(INPUT_B_POSTCODE_FORMAT_MIX, file = "./tests/testthat/testdata/match_test_input_b_postcode_format_mix.rds")




# OUTPUT TEST01- INPUT_A_SINGLE_CASE v INPUT_B_INC_EXACT_MATCH --------------------------------
# output will be a single row showing the exact match
OUTPUT_TEST01 <- data.frame(
  ID = "1",
  ID_TWO = "1",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 1,
  MATCH_SCORE = 1
)

# format dataset
OUTPUT_TEST01 <- OUTPUT_TEST01 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
saveRDS(OUTPUT_TEST01, file = "./tests/testthat/testdata/match_test_output_test01.rds")


# OUTPUT TEST02- INPUT_A_SINGLE_CASE v INPUT_B_EXCL_EXACT_MATCH --------------------------------
# output will be multiple rows for all the confident matches
# output dataset will include all fields from both datasets
OUTPUT_TEST02 <- data.frame(
  ID = "1",
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  DOB = "19720824",
  POSTCODE = "NE158NY",
  ID_TWO = "2",
  FORENAME_TWO = "MICHAEL",
  SURNAME_TWO = "CORLEON",
  DOB_TWO = "19720824",
  POSTCODE_TWO = "NE158NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9938,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = 0.9583,
  DOB_DIFFERENCE = 0,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = 1.0000,
  DF2_NOTES = "Confident - surname close"
)
OUTPUT_TEST02 <- rbind(OUTPUT_TEST02, data.frame(
  ID = "1",
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  DOB = "19720824",
  POSTCODE = "NE158NY",
  ID_TWO = "3",
  FORENAME_TWO = "MICHEAL",
  SURNAME_TWO = "CORLEONE",
  DOB_TWO = "19720824",
  POSTCODE_TWO = "NE158NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9857,
  FORENAME_SCORE = 0.9524,
  SURNAME_SCORE = 1.0000,
  DOB_DIFFERENCE = 0,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = 1.0000,
  DF2_NOTES = "Confident - forename close"
))
OUTPUT_TEST02 <- rbind(OUTPUT_TEST02, data.frame(
  ID = "1",
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  DOB = "19720824",
  POSTCODE = "NE158NY",
  ID_TWO = "4",
  FORENAME_TWO = "MICHAEL",
  SURNAME_TWO = "CORLEONE",
  DOB_TWO = "19720824",
  POSTCODE_TWO = "NE61JS",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9464,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = 1.0000,
  DOB_DIFFERENCE = 0,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = 0.6429,
  DF2_NOTES = "Confident - postcode close"
))
OUTPUT_TEST02 <- rbind(OUTPUT_TEST02, data.frame(
  ID = "1",
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  DOB = "19720824",
  POSTCODE = "NE158NY",
  ID_TWO = "5",
  FORENAME_TWO = "MICHAEL",
  SURNAME_TWO = "CORLEONE",
  DOB_TWO = "19720825",
  POSTCODE_TWO = "NE158NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9520,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = 1.0000,
  DOB_DIFFERENCE = 1,
  DOB_SCORE = 0.88,
  POSTCODE_SCORE = 1.0000,
  DF2_NOTES = "Confident - DOB 1 out"
))
OUTPUT_TEST02 <- rbind(OUTPUT_TEST02, data.frame(
  ID = "1",
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  DOB = "19720824",
  POSTCODE = "NE158NY",
  ID_TWO = "6",
  FORENAME_TWO = "MICHAEL",
  SURNAME_TWO = "CORLEONE",
  DOB_TWO = "19730825",
  POSTCODE_TWO = "NE158NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.90000,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = 1.0000,
  DOB_DIFFERENCE = 2,
  DOB_SCORE = 0.75,
  POSTCODE_SCORE = 1.0000,
  DF2_NOTES = "Confident - DOB 2 out"
))
OUTPUT_TEST02 <- rbind(OUTPUT_TEST02, data.frame(
  ID = "1",
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  DOB = "19720824",
  POSTCODE = "NE158NY",
  ID_TWO = "7",
  FORENAME_TWO = "MICHAEL",
  SURNAME_TWO = "CORLEONE",
  DOB_TWO = "19720824",
  POSTCODE_TWO = NA,
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.85000,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = 1.0000,
  DOB_DIFFERENCE = 0,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = NA,
  DF2_NOTES = "Confident - postcode missing"
))
OUTPUT_TEST02 <- rbind(OUTPUT_TEST02, data.frame(
  ID = "1",
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  DOB = "19720824",
  POSTCODE = "NE158NY",
  ID_TWO = "8",
  FORENAME_TWO = "MICHAEL",
  SURNAME_TWO = NA,
  DOB_TWO = "19720824",
  POSTCODE_TWO = "NE158NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.85000,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = NA,
  DOB_DIFFERENCE = 0,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = 1.0000,
  DF2_NOTES = "Confident - surname missing"
))
OUTPUT_TEST02 <- rbind(OUTPUT_TEST02, data.frame(
  ID = "1",
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  DOB = "19720824",
  POSTCODE = "NE158NY",
  ID_TWO = "9",
  FORENAME_TWO = "MICHEAL",
  SURNAME_TWO = "CORLEON",
  DOB_TWO = "19720825",
  POSTCODE_TWO = "NE15NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9243,
  FORENAME_SCORE = 0.9524,
  SURNAME_SCORE = 0.9583,
  DOB_DIFFERENCE = 1,
  DOB_SCORE = 0.88,
  POSTCODE_SCORE = 0.9524,
  DF2_NOTES = "Confident - all close"
))

# format dataset
OUTPUT_TEST02 <- OUTPUT_TEST02 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
saveRDS(OUTPUT_TEST02, file = "./tests/testthat/testdata/match_test_output_test02.rds")


# OUTPUT TEST03- INPUT_A_MULTIPLE_CASE v INPUT_B_INC_EXACT_MATCH --------------------------------
# output will be a single row showing the exact match
OUTPUT_TEST03 <- data.frame(
  ID = "1",
  FORENAME = "MICHAEL",
  SURNAME = "CORLEONE",
  DOB = "19720824",
  POSTCODE = "NE158NY",
  ID_TWO = "1",
  FORENAME_TWO = "MICHAEL",
  SURNAME_TWO = "CORLEONE",
  DOB_TWO = "19720824",
  POSTCODE_TWO = "NE158NY",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 1,
  MATCH_SCORE = 1
)
OUTPUT_TEST03 <- rbind(OUTPUT_TEST03, data.frame(
  ID = "2",
  FORENAME = "LUCA",
  SURNAME = "BRASI",
  DOB = "19720101",
  POSTCODE = "NE61JS",
  ID_TWO = NA,
  FORENAME_TWO = NA,
  SURNAME_TWO = NA,
  DOB_TWO = NA,
  POSTCODE_TWO = NA,
  MATCH_TYPE = "No Match",
  MATCH_COUNT = 0,
  MATCH_SCORE = 0
))

# format dataset
OUTPUT_TEST03 <- OUTPUT_TEST03 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
saveRDS(OUTPUT_TEST03, file = "./tests/testthat/testdata/match_test_output_test03.rds")


# OUTPUT TEST04- INPUT_A_NO_MATCH_CASE v INPUT_B_INC_EXACT_MATCH --------------------------------
# output will be a single row showing the exact match
OUTPUT_TEST04 <- data.frame(
  ID = "2",
  ID_TWO = NA,
  MATCH_TYPE = "No Match",
  MATCH_COUNT = 0,
  MATCH_SCORE = 0
)

# format dataset
OUTPUT_TEST04 <- OUTPUT_TEST04 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
saveRDS(OUTPUT_TEST04, file = "./tests/testthat/testdata/match_test_output_test04.rds")


# OUTPUT TEST05- INPUT_A_SINGLE_CASE v INPUT_B_FORMAT_DATES --------------------------------
# output will be a single row showing the exact match
OUTPUT_TEST05 <- data.frame(
  ID = "1",
  ID_TWO = "1",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
)
OUTPUT_TEST05 <- rbind(OUTPUT_TEST05, data.frame(
  ID = "1",
  ID_TWO = "2",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
OUTPUT_TEST05 <- rbind(OUTPUT_TEST05, data.frame(
  ID = "1",
  ID_TWO = "3",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
OUTPUT_TEST05 <- rbind(OUTPUT_TEST05, data.frame(
  ID = "1",
  ID_TWO = "4",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
OUTPUT_TEST05 <- rbind(OUTPUT_TEST05, data.frame(
  ID = "1",
  ID_TWO = "5",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
OUTPUT_TEST05 <- rbind(OUTPUT_TEST05, data.frame(
  ID = "1",
  ID_TWO = "6",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))

# format dataset
OUTPUT_TEST05 <- OUTPUT_TEST05 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
saveRDS(OUTPUT_TEST05, file = "./tests/testthat/testdata/match_test_output_test05.rds")


# OUTPUT TEST05- INPUT_A_SINGLE_CASE v INPUT_B_FORMAT_DATES --------------------------------
# output will be a single row showing the exact match
OUTPUT_TEST06 <- data.frame(
  ID = "1",
  ID_TWO = "1",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
)
OUTPUT_TEST06 <- rbind(OUTPUT_TEST06, data.frame(
  ID = "1",
  ID_TWO = "2",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
OUTPUT_TEST06 <- rbind(OUTPUT_TEST06, data.frame(
  ID = "1",
  ID_TWO = "3",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
OUTPUT_TEST06 <- rbind(OUTPUT_TEST06, data.frame(
  ID = "1",
  ID_TWO = "4",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
OUTPUT_TEST06 <- rbind(OUTPUT_TEST06, data.frame(
  ID = "1",
  ID_TWO = "5",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
OUTPUT_TEST06 <- rbind(OUTPUT_TEST06, data.frame(
  ID = "1",
  ID_TWO = "6",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))

# format dataset
OUTPUT_TEST06 <- OUTPUT_TEST06 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
saveRDS(OUTPUT_TEST06, file = "./tests/testthat/testdata/match_test_output_test06.rds")
