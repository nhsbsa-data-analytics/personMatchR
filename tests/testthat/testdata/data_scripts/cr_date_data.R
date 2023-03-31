#' Test cases for format_date function
#' Test dataset creation
#'
#' BUild the dataframes for the test cases
#'


# Test Case Dataset ---------------------------------------------------------------------------
# Test dates: attempt to cover all scenarios
DOB_TEST <- c(
  "25/12/2000",
  "25-12-2000",
  "25 12 2000",
  "25/DEC/2000",
  "25-DEC-2000",
  "25 DEC 2000",
  "25/dec/2000",
  "25-dec-2000",
  "25 dec 2000",
  "25/DECEMBER/2000",
  "25-DECEMBER-2000",
  "25 DECEMBER 2000",
  "25/december/2000",
  "25-december-2000",
  "25 december 2000",
  "2000/12/25",
  "2000-12-25",
  "2000 12 25",
  "2000/DEC/25",
  "2000-DEC-25",
  "2000 DEC 25",
  "2000/DECEMBER/25",
  "2000-DECEMBER-25",
  "2000 DECEMBER 25",
  "12/25/2000",
  "12-25-2000",
  "12 25 2000",
  "December/25/2000",
  "December-25-2000",
  "December 25 2000",
  "12252000",
  "25122000",
  "20001225",
  "20002512",
  "25/DCM/2000",
  "25/25/2000",
  NA,
  ""
)


# Correct dates: Output that should be achieved by format function
DOB_RESULT <- c(
  "20001225", # "25/12/2000",
  "20001225", # "25-12-2000",
  "20001225", # "25 12 2000",
  "20001225", # "25/DEC/2000",
  "20001225", # "25-DEC-2000",
  "20001225", # "25 DEC 2000",
  "20001225", # "25/dec/2000",
  "20001225", # "25-dec-2000",
  "20001225", # "25 dec 2000",
  "20001225", # "25/DECEMBER/2000",
  "20001225", # "25-DECEMBER-2000",
  "20001225", # "25 DECEMBER 2000",
  "20001225", # "25/december/2000",
  "20001225", # "25-december-2000",
  "20001225", # "25 december 2000",
  "20001225", # "2000/12/25",
  "20001225", # "2000-12-25",
  "20001225", # "2000 12 25",
  "20001225", # "2000/DEC/25",
  "20001225", # "2000-DEC-25",
  "20001225", # "2000 DEC 25",
  "20001225", # "2000/DECEMBER/25",
  "20001225", # "2000-DECEMBER-25",
  "20001225", # "2000 DECEMBER 25",
  "20001225", # "12/25/2000",
  "20001225", # "12-25-2000",
  "20001225", # "12 25 2000",
  "20001225", # "December/25/2000",
  "20001225", # "December-25-2000",
  "20001225", # "December 25 2000",
  "20001225", # "12252000",
  "20001225", # "25122000",
  "20001225", # "20001225",
  "20001225", # "20002512",
  NA, # "25/DCM/2000",
  NA, # "25/25/2000",
  NA, # NA,
  NA # ""
)

DOB_TEST_INTEGER <- c(
  20002512,
  20000131,
  20003101,
  20003131,
  251220,
  NA
)

DOB_RESULT_INTEGER <- c(
  "20001225",
  "20000131",
  "20000131",
  NA,
  "20201225",
  NA
)

DOB_TEST_DATE <- c(
  as.Date("20001225","%Y%m%d"),
  NA
)

DOB_RESULT_DATE <- c(
  "20001225",
  NA
)

# Save files to test folder
test_date_input <- data.frame(ID = seq(1:length(DOB_TEST)), DOB = DOB_TEST)
save(test_date_input, file = "./tests/testthat/testdata/test_date_input.rda")
test_date_expected <- data.frame(ID = seq(1:length(DOB_RESULT)), DOB = DOB_RESULT)
save(test_date_expected, file = "./tests/testthat/testdata/test_date_expected.rda")

test_date_input_integer <- data.frame(ID = seq(1:length(DOB_TEST_INTEGER)), DOB = DOB_TEST_INTEGER)
save(test_date_input_integer, file = "./tests/testthat/testdata/test_date_input_integer.rda")
test_date_expected_integer <- data.frame(ID = seq(1:length(DOB_RESULT_INTEGER)), DOB = DOB_RESULT_INTEGER)
save(test_date_expected_integer, file = "./tests/testthat/testdata/test_date_expected_integer.rda")

test_date_input_date <- data.frame(ID = seq(1:length(DOB_TEST_DATE)), DOB = DOB_TEST_DATE)
save(test_date_input_date, file = "./tests/testthat/testdata/test_date_input_date.rda")
test_date_expected_date <- data.frame(ID = seq(1:length(DOB_RESULT_DATE)), DOB = DOB_RESULT_DATE)
save(test_date_expected_date, file = "./tests/testthat/testdata/test_date_expected_date.rda")
