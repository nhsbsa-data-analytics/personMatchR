#' Test cases for calc_match_person function
#' Test dataset creation
#'
#' BUild the dataframes for the test cases
#'


# INPUT_A_SINGLE_CASE ---------------------------------------------------------------------------
# file for "left hand side" of testing
# this test dataset will only include a single test case
# a single record may identify possible issues with the process after an exact match is found
# if this is exactly matched there will be nothing to pass to the rest of the tests

match_test_input_a_single <- data.frame(
  ID = "1",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24"
)

save(match_test_input_a_single, file = "./tests/testthat/testdata/match_test_input_a_single.rda")



# INPUT_A_MULTIPLE_CASE -------------------------------------------------------------------------
# file for "left hand side" of testing
# adds an additional test case that will not appear in any of other test cases so will alway "no match"

match_test_input_a_multiple <- rbind(match_test_input_a_single, data.frame(
  ID = "2",
  SURNAME = "BRASI",
  FORENAME = "LUCA",
  POSTCODE = "NE6 1JS",
  DOB = "1972-01-01"
))

save(match_test_input_a_multiple, file = "./tests/testthat/testdata/match_test_input_a_multiple.rda")



# INPUT_A_NO_MATCH_CASE -------------------------------------------------------------------------
# file for "left hand side" of testing
# case where there is only a no match

match_test_input_a_no_match <- data.frame(
  ID = "2",
  SURNAME = "BRASI",
  FORENAME = "LUCA",
  POSTCODE = "NE6 1JS",
  DOB = "1972-01-01"
)

save(match_test_input_a_no_match, file = "./tests/testthat/testdata/match_test_input_a_no_match.rda")



# INPUT_B_INC_EXACT_MATCH ---------------------------------------------------------------------
# file for "right hand side" of testing
# will include multiple potential matches
# including one which is an exact match
match_test_input_b_inc_exact_match <- data.frame(
  ID = "1",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
)
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "2",
  SURNAME = "CORLEON",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Confident - surname close"
))
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "3",
  SURNAME = "CORLEONE",
  FORENAME = "MICHEAL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Confident - forename close"
))
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "4",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE6 1JS",
  DOB = "1972-08-24",
  NOTES = "Confident - postcode close"
))
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "5",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-25",
  NOTES = "Confident - DOB 1 out"
))
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "6",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1973-08-25",
  NOTES = "Confident - DOB 2 out"
))
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "7",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = NA,
  DOB = "1972-08-24",
  NOTES = "Confident - postcode missing"
))
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "8",
  SURNAME = NA,
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Confident - surname missing"
))
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "9",
  SURNAME = "CORLEON",
  FORENAME = "MICHEAL",
  POSTCODE = "NE1 5NY",
  DOB = "1972-08-24",
  NOTES = "Confident - DOB match and others close"
))
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "10",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1973-09-25",
  NOTES = "No match - DOB out by 3+"
))
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "11",
  SURNAME = "CORLEONE",
  FORENAME = "FREDO",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "No match - forename different"
))
match_test_input_b_inc_exact_match <- rbind(match_test_input_b_inc_exact_match, data.frame(
  ID = "12",
  SURNAME = "HAGAN",
  FORENAME = "TOM",
  POSTCODE = "SW1A 1AA",
  DOB = "1991-03-08",
  NOTES = "No match - all different"
))

save(match_test_input_b_inc_exact_match, file = "./tests/testthat/testdata/match_test_input_b_inc_exact_match.rda")


# INPUT_B_EXCL_EXACT_MATCH --------------------------------------------------------------------
# file for "right hand side" of testing
# will include multiple potential matches
# none of these will be an exact match though

match_test_input_b_excl_exact_match <- match_test_input_b_inc_exact_match %>%
  dplyr::filter(ID != "1")

save(match_test_input_b_excl_exact_match, file = "./tests/testthat/testdata/match_test_input_b_excl_exact_match.rda")



# INPUT_B_DATE_FORMAT_MIX ---------------------------------------------------------------------
# file for "right hand side" of testing
# will include multiple potential matches
# all should be an exact match but only with different formatting of the DOB string
match_test_input_b_date_format_mix <- data.frame(
  ID = "1",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
)
match_test_input_b_date_format_mix <- rbind(match_test_input_b_date_format_mix, data.frame(
  ID = "2",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-24-08",
  NOTES = "Exact match"
))
match_test_input_b_date_format_mix <- rbind(match_test_input_b_date_format_mix, data.frame(
  ID = "3",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "24/08/1972",
  NOTES = "Exact match"
))
match_test_input_b_date_format_mix <- rbind(match_test_input_b_date_format_mix, data.frame(
  ID = "4",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "24-aug-1972",
  NOTES = "Exact match"
))
match_test_input_b_date_format_mix <- rbind(match_test_input_b_date_format_mix, data.frame(
  ID = "5",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "08-24-1972",
  NOTES = "Exact match"
))
match_test_input_b_date_format_mix <- rbind(match_test_input_b_date_format_mix, data.frame(
  ID = "6",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "august 24 1972",
  NOTES = "Exact match"
))

save(match_test_input_b_date_format_mix, file = "./tests/testthat/testdata/match_test_input_b_date_format_mix.rda")



# INPUT_B_POSTCODE_FORMAT_MIX -----------------------------------------------------------------
# file for "right hand side" of testing
# will include multiple potential matches
# all should be an exact match but only with different formatting of the POSTCODE string

match_test_input_b_postcode_format_mix <- data.frame(
  ID = "1",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15 8NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
)
match_test_input_b_postcode_format_mix <- rbind(match_test_input_b_postcode_format_mix, data.frame(
  ID = "2",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE158NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
))
match_test_input_b_postcode_format_mix <- rbind(match_test_input_b_postcode_format_mix, data.frame(
  ID = "3",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE15-8NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
))
match_test_input_b_postcode_format_mix <- rbind(match_test_input_b_postcode_format_mix, data.frame(
  ID = "4",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NEI58NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
))
match_test_input_b_postcode_format_mix <- rbind(match_test_input_b_postcode_format_mix, data.frame(
  ID = "5",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NEl58NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
))
match_test_input_b_postcode_format_mix <- rbind(match_test_input_b_postcode_format_mix, data.frame(
  ID = "6",
  SURNAME = "CORLEONE",
  FORENAME = "MICHAEL",
  POSTCODE = "NE1  58NY",
  DOB = "1972-08-24",
  NOTES = "Exact match"
))

save(match_test_input_b_postcode_format_mix, file = "./tests/testthat/testdata/match_test_input_b_postcode_format_mix.rda")




# OUTPUT TEST01- INPUT_A_SINGLE_CASE v INPUT_B_INC_EXACT_MATCH --------------------------------
# output will be a single row showing the exact match
match_test_output_test01 <- data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "1",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 1,
  MATCH_SCORE = 1
)

# format dataset
match_test_output_test01 <- match_test_output_test01 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
save(match_test_output_test01, file = "./tests/testthat/testdata/match_test_output_test01.rda")


# OUTPUT TEST02- INPUT_A_SINGLE_CASE v INPUT_B_EXCL_EXACT_MATCH --------------------------------
# output will be multiple rows for all the confident matches
# output dataset will include all fields from both datasets
match_test_output_test02 <- data.frame(
  DF1_INPUT_ID = "1",
  DF1_INPUT_FORENAME = "MICHAEL",
  DF1_INPUT_SURNAME = "CORLEONE",
  DF1_INPUT_DOB = "19720824",
  DF1_INPUT_POSTCODE = "NE158NY",
  DF2_INPUT_ID = "2",
  DF2_INPUT_FORENAME = "MICHAEL",
  DF2_INPUT_SURNAME = "CORLEON",
  DF2_INPUT_DOB = "19720824",
  DF2_INPUT_POSTCODE = "NE158NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9962,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = 0.975,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = 1.0000,
  DF2_NOTES = "Confident - surname close"
)
match_test_output_test02 <- rbind(match_test_output_test02, data.frame(
  DF1_INPUT_ID = "1",
  DF1_INPUT_FORENAME = "MICHAEL",
  DF1_INPUT_SURNAME = "CORLEONE",
  DF1_INPUT_DOB = "19720824",
  DF1_INPUT_POSTCODE = "NE158NY",
  DF2_INPUT_ID = "3",
  DF2_INPUT_FORENAME = "MICHEAL",
  DF2_INPUT_SURNAME = "CORLEONE",
  DF2_INPUT_DOB = "19720824",
  DF2_INPUT_POSTCODE = "NE158NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9914,
  FORENAME_SCORE = 0.9714,
  SURNAME_SCORE = 1.0000,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = 1.0000,
  DF2_NOTES = "Confident - forename close"
))
match_test_output_test02 <- rbind(match_test_output_test02, data.frame(
  DF1_INPUT_ID = "1",
  DF1_INPUT_FORENAME = "MICHAEL",
  DF1_INPUT_SURNAME = "CORLEONE",
  DF1_INPUT_DOB = "19720824",
  DF1_INPUT_POSTCODE = "NE158NY",
  DF2_INPUT_ID = "4",
  DF2_INPUT_FORENAME = "MICHAEL",
  DF2_INPUT_SURNAME = "CORLEONE",
  DF2_INPUT_DOB = "19720824",
  DF2_INPUT_POSTCODE = "NE61JS",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9571,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = 1.0000,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = 0.7143,
  DF2_NOTES = "Confident - postcode close"
))
match_test_output_test02 <- rbind(match_test_output_test02, data.frame(
  DF1_INPUT_ID = "1",
  DF1_INPUT_FORENAME = "MICHAEL",
  DF1_INPUT_SURNAME = "CORLEONE",
  DF1_INPUT_DOB = "19720824",
  DF1_INPUT_POSTCODE = "NE158NY",
  DF2_INPUT_ID = "5",
  DF2_INPUT_FORENAME = "MICHAEL",
  DF2_INPUT_SURNAME = "CORLEONE",
  DF2_INPUT_DOB = "19720825",
  DF2_INPUT_POSTCODE = "NE158NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9520,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = 1.0000,
  DOB_SCORE = 0.88,
  POSTCODE_SCORE = 1.0000,
  DF2_NOTES = "Confident - DOB 1 out"
))
match_test_output_test02 <- rbind(match_test_output_test02, data.frame(
  DF1_INPUT_ID = "1",
  DF1_INPUT_FORENAME = "MICHAEL",
  DF1_INPUT_SURNAME = "CORLEONE",
  DF1_INPUT_DOB = "19720824",
  DF1_INPUT_POSTCODE = "NE158NY",
  DF2_INPUT_ID = "6",
  DF2_INPUT_FORENAME = "MICHAEL",
  DF2_INPUT_SURNAME = "CORLEONE",
  DF2_INPUT_DOB = "19730825",
  DF2_INPUT_POSTCODE = "NE158NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9000,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = 1.0000,
  DOB_SCORE = 0.75,
  POSTCODE_SCORE = 1.0000,
  DF2_NOTES = "Confident - DOB 2 out"
))
match_test_output_test02 <- rbind(match_test_output_test02, data.frame(
  DF1_INPUT_ID = "1",
  DF1_INPUT_FORENAME = "MICHAEL",
  DF1_INPUT_SURNAME = "CORLEONE",
  DF1_INPUT_DOB = "19720824",
  DF1_INPUT_POSTCODE = "NE158NY",
  DF2_INPUT_ID = "7",
  DF2_INPUT_FORENAME = "MICHAEL",
  DF2_INPUT_SURNAME = "CORLEONE",
  DF2_INPUT_DOB = "19720824",
  DF2_INPUT_POSTCODE = NA,
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.8500,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = 1.0000,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = NA,
  DF2_NOTES = "Confident - postcode missing"
))
match_test_output_test02 <- rbind(match_test_output_test02, data.frame(
  DF1_INPUT_ID = "1",
  DF1_INPUT_FORENAME = "MICHAEL",
  DF1_INPUT_SURNAME = "CORLEONE",
  DF1_INPUT_DOB = "19720824",
  DF1_INPUT_POSTCODE = "NE158NY",
  DF2_INPUT_ID = "8",
  DF2_INPUT_FORENAME = "MICHAEL",
  DF2_INPUT_SURNAME = NA,
  DF2_INPUT_DOB = "19720824",
  DF2_INPUT_POSTCODE = "NE158NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.8500,
  FORENAME_SCORE = 1.0000,
  SURNAME_SCORE = NA,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = 1.0000,
  DF2_NOTES = "Confident - surname missing"
))
match_test_output_test02 <- rbind(match_test_output_test02, data.frame(
  DF1_INPUT_ID = "1",
  DF1_INPUT_FORENAME = "MICHAEL",
  DF1_INPUT_SURNAME = "CORLEONE",
  DF1_INPUT_DOB = "19720824",
  DF1_INPUT_POSTCODE = "NE158NY",
  DF2_INPUT_ID = "9",
  DF2_INPUT_FORENAME = "MICHEAL",
  DF2_INPUT_SURNAME = "CORLEON",
  DF2_INPUT_DOB = "19720824",
  DF2_INPUT_POSTCODE = "NE15NY",
  MATCH_TYPE = "Confident",
  MATCH_COUNT = 8,
  MATCH_SCORE = 0.9834,
  FORENAME_SCORE = 0.9714,
  SURNAME_SCORE = 0.975,
  DOB_SCORE = 1.00,
  POSTCODE_SCORE = 0.9714,
  DF2_NOTES = "Confident - DOB match and others close"
))

# format dataset
match_test_output_test02 <- match_test_output_test02 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
save(match_test_output_test02, file = "./tests/testthat/testdata/match_test_output_test02.rda")


# OUTPUT TEST03- INPUT_A_MULTIPLE_CASE v INPUT_B_INC_EXACT_MATCH --------------------------------
# output will be a single row showing the exact match
match_test_output_test03 <- data.frame(
  DF1_INPUT_ID = "1",
  DF1_INPUT_FORENAME = "MICHAEL",
  DF1_INPUT_SURNAME = "CORLEONE",
  DF1_INPUT_DOB = "19720824",
  DF1_INPUT_POSTCODE = "NE158NY",
  DF2_INPUT_ID = "1",
  DF2_INPUT_FORENAME = "MICHAEL",
  DF2_INPUT_SURNAME = "CORLEONE",
  DF2_INPUT_DOB = "19720824",
  DF2_INPUT_POSTCODE = "NE158NY",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 1,
  MATCH_SCORE = 1
)
match_test_output_test03 <- rbind(match_test_output_test03, data.frame(
  DF1_INPUT_ID = "2",
  DF1_INPUT_FORENAME = "LUCA",
  DF1_INPUT_SURNAME = "BRASI",
  DF1_INPUT_DOB = "19720101",
  DF1_INPUT_POSTCODE = "NE61JS",
  DF2_INPUT_ID = NA,
  DF2_INPUT_FORENAME = NA,
  DF2_INPUT_SURNAME = NA,
  DF2_INPUT_DOB = NA,
  DF2_INPUT_POSTCODE = NA,
  MATCH_TYPE = "No Match",
  MATCH_COUNT = 0,
  MATCH_SCORE = 0
))

# format dataset
match_test_output_test03 <- match_test_output_test03 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
save(match_test_output_test03, file = "./tests/testthat/testdata/match_test_output_test03.rda")


# OUTPUT TEST04- INPUT_A_NO_MATCH_CASE v INPUT_B_INC_EXACT_MATCH --------------------------------
# output will be a single row showing the exact match
match_test_output_test04 <- data.frame(
  DF1_INPUT_ID = "2",
  DF2_INPUT_ID = NA,
  MATCH_TYPE = "No Match",
  MATCH_COUNT = 0,
  MATCH_SCORE = 0
)

# format dataset
match_test_output_test04 <- match_test_output_test04 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
save(match_test_output_test04, file = "./tests/testthat/testdata/match_test_output_test04.rda")


# OUTPUT TEST05- INPUT_A_SINGLE_CASE v INPUT_B_FORMAT_DATES --------------------------------
# output will be a single row showing the exact match
match_test_output_test05 <- data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "1",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
)
match_test_output_test05 <- rbind(match_test_output_test05, data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "2",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
match_test_output_test05 <- rbind(match_test_output_test05, data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "3",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
match_test_output_test05 <- rbind(match_test_output_test05, data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "4",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
match_test_output_test05 <- rbind(match_test_output_test05, data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "5",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
match_test_output_test05 <- rbind(match_test_output_test05, data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "6",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))

# format dataset
match_test_output_test05 <- match_test_output_test05 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
save(match_test_output_test05, file = "./tests/testthat/testdata/match_test_output_test05.rda")


# OUTPUT TEST05- INPUT_A_SINGLE_CASE v INPUT_B_FORMAT_DATES --------------------------------
# output will be a single row showing the exact match
match_test_output_test06 <- data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "1",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
)
match_test_output_test06 <- rbind(match_test_output_test06, data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "2",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
match_test_output_test06 <- rbind(match_test_output_test06, data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "3",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
match_test_output_test06 <- rbind(match_test_output_test06, data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "4",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
match_test_output_test06 <- rbind(match_test_output_test06, data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "5",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))
match_test_output_test06 <- rbind(match_test_output_test06, data.frame(
  DF1_INPUT_ID = "1",
  DF2_INPUT_ID = "6",
  MATCH_TYPE = "Exact",
  MATCH_COUNT = 6,
  MATCH_SCORE = 1
))

# format dataset
match_test_output_test06 <- match_test_output_test06 %>%
  mutate(MATCH_COUNT = as.integer(MATCH_COUNT))

# save output
save(match_test_output_test06, file = "./tests/testthat/testdata/match_test_output_test06.rda")
