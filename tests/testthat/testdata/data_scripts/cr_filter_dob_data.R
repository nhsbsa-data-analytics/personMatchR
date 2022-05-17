#' Test cases for filter_dob function
#' Test dataset creation
#'
#' BUild the dataframes for the test cases
#'


# Test Case Dataset ---------------------------------------------------------------------------
# create test input to cover all scenarios
TEST_INPUT <- data.frame(DOB_A = "20001225", DOB_B = "20001225") # date strings identical
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "10001225")) # 1 character difference (position 1)
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "21001225")) # 1 character difference (position 2)
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "20101225")) # 1 character difference (position 3)
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "20011225")) # 1 character difference (position 4)
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "20000225")) # 1 character difference (position 5)
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "20001125")) # 1 character difference (position 6)
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "20001215")) # 1 character difference (position 7)
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "20001226")) # 1 character difference (position 8)
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "20221225")) # 2 character difference
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "20000915")) # 3 character difference
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "19991225")) # 4 character difference
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "19990225")) # 5 character difference
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "19990725")) # 6 character difference
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "19990715")) # 7 character difference
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "19990716")) # 8 character difference
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "20002512")) # day/month swap (4 character difference)
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "20001225", DOB_B = "20221225")) # latest year captured (2 character difference)
TEST_INPUT <- rbind(TEST_INPUT, data.frame(DOB_A = "19901225", DOB_B = "20222512")) # latest year captured (4 character difference)



# create test input to cover all scenarios
TEST_OUTPUT <- data.frame(DOB_A = "20001225", DOB_B = "20001225", DIFF_DOB = 0) # date strings identical
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "10001225", DIFF_DOB = 1)) # 1 character difference (position 1)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "21001225", DIFF_DOB = 1)) # 1 character difference (position 2)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20101225", DIFF_DOB = 1)) # 1 character difference (position 3)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20011225", DIFF_DOB = 1)) # 1 character difference (position 4)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20000225", DIFF_DOB = 1)) # 1 character difference (position 5)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20001125", DIFF_DOB = 1)) # 1 character difference (position 6)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20001215", DIFF_DOB = 1)) # 1 character difference (position 7)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20001226", DIFF_DOB = 1)) # 1 character difference (position 8)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20221225", DIFF_DOB = 2)) # 2 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20000915", DIFF_DOB = 3)) # 3 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "19991225", DIFF_DOB = 4)) # 4 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "19990225", DIFF_DOB = 5)) # 5 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "19990725", DIFF_DOB = 6)) # 6 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "19990715", DIFF_DOB = 7)) # 7 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "19990716", DIFF_DOB = 8)) # 8 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20002512", DIFF_DOB = 4)) # day/month swap (4 character difference)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20221225", DIFF_DOB = 2)) # latest year captured (2 character difference)
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "19901225", DOB_B = "20222512", DIFF_DOB = 4)) # latest year captured (4 character difference)



# Save files to test folder
saveRDS(TEST_INPUT, file = "./tests/testthat/testdata/test_filter_dob_input.rds")
saveRDS(TEST_OUTPUT, file = "./tests/testthat/testdata/test_filter_dob_expected.rds")
