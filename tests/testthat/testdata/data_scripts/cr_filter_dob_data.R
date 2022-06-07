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
TEST_OUTPUT <- data.frame(DOB_A = "20001225", DOB_B = "20001225", DOB_SCORE = 1.00) # date strings identical
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "10001225", DOB_SCORE = 0.88)) # 1 character difference (position 1)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "21001225", DOB_SCORE = 0.88)) # 1 character difference (position 2)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20101225", DOB_SCORE = 0.88)) # 1 character difference (position 3)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20011225", DOB_SCORE = 0.88)) # 1 character difference (position 4)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20000225", DOB_SCORE = 0.88)) # 1 character difference (position 5)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20001125", DOB_SCORE = 0.88)) # 1 character difference (position 6)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20001215", DOB_SCORE = 0.88)) # 1 character difference (position 7)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20001226", DOB_SCORE = 0.88)) # 1 character difference (position 8)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20221225", DOB_SCORE = 0.75)) # 2 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20000915", DOB_SCORE = 3)) # 3 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "19991225", DOB_SCORE = 4)) # 4 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "19990225", DOB_SCORE = 5)) # 5 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "19990725", DOB_SCORE = 6)) # 6 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "19990715", DOB_SCORE = 7)) # 7 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "19990716", DOB_SCORE = 8)) # 8 character difference
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20002512", DOB_SCORE = 4)) # day/month swap (4 character difference)
TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "20001225", DOB_B = "20221225", DOB_SCORE = 0.75)) # latest year captured (2 character difference)
# TEST_OUTPUT <- rbind(TEST_OUTPUT, data.frame(DOB_A = "19901225", DOB_B = "20222512", DOB_SCORE = 4)) # latest year captured (4 character difference)



# Save files to test folder
saveRDS(TEST_INPUT, file = "./tests/testthat/testdata/test_filter_dob_input.rds")
saveRDS(TEST_OUTPUT, file = "./tests/testthat/testdata/test_filter_dob_expected.rds")
