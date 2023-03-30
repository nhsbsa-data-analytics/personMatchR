#' Test cases for filter_name function
#' Test dataset creation
#'
#' BUild the dataframes for the test cases
#'


# Test Case Dataset ---------------------------------------------------------------------------
# create test input to cover all scenarios
TEST_INPUT <- data.frame(NAME_A = "MICHAEL", NAME_B = "MICHAEL") # names are identical
TEST_INPUT <- rbind(TEST_INPUT, data.frame(NAME_A = "MICHAEL", NAME_B = "BOB")) # names are completely different
TEST_INPUT <- rbind(TEST_INPUT, data.frame(NAME_A = "MICHAEL", NAME_B = "MATT")) # share first letter
TEST_INPUT <- rbind(TEST_INPUT, data.frame(NAME_A = "MICHAEL", NAME_B = "LIBBY")) # share second letter
TEST_INPUT <- rbind(TEST_INPUT, data.frame(NAME_A = "MICHAEL", NAME_B = "PAUL")) # share last letter
TEST_INPUT <- rbind(TEST_INPUT, data.frame(NAME_A = "MICHAEL", NAME_B = "PAUL")) # share last letter
TEST_INPUT <- rbind(TEST_INPUT, data.frame(NAME_A = "REBECCA", NAME_B = "BEC")) # share last letter
TEST_INPUT <- rbind(TEST_INPUT, data.frame(NAME_A = "LIZ", NAME_B = "ELIZABETH")) # share last letter


# create test input to cover all scenarios
TEST_OUTPUT <- data.frame(NAME_A = "MICHAEL", NAME_B = "MICHAEL") # names are identical
# TEST_OUTPUT <- rbind(TEST_INPUT, data.frame(NAME_A = "MICHAEL", NAME_B = "BOB")) # names are completely different
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(NAME_A = "MICHAEL", NAME_B = "MATT")) # share first letter
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(NAME_A = "MICHAEL", NAME_B = "LIBBY")) # share second letter
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(NAME_A = "MICHAEL", NAME_B = "PAUL")) # share last letter
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(NAME_A = "MICHAEL", NAME_B = "PAUL")) # share last letter
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(NAME_A = "REBECCA", NAME_B = "BEC")) # share last letter
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(NAME_A = "LIZ", NAME_B = "ELIZABETH")) # share last letter



# Save files to test folder
test_filter_name_input <- TEST_INPUT
save(test_filter_name_input, file = "./tests/testthat/testdata/test_filter_name_input.rda")
test_filter_name_expected <- TEST_OUTPUT
save(test_filter_name_expected, file = "./tests/testthat/testdata/test_filter_name_expected.rda")
