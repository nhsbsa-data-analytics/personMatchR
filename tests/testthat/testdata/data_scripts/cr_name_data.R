#' Test cases for format_name function
#' Test dataset creation
#'
#' BUild the dataframes for the test cases
#'


# Test Case Dataset ---------------------------------------------------------------------------

# Test names: attempt to cover all scenarios
NAME_TEST <- c(
  "STEVEN", # valid and formatted as expected
  "steven", # requires converting to uppercase
  "John-Paul", # requires non alphabetic characters removed
  "JOHN paul", # requires non alphabetic characters removed
  "David John-Paul", # requires non alphabetic characters removed
  "peter9", # requires non alphabetic characters removed
  "Mc'Donald", # requires non alphabetic characters removed
  "", # Blank values are skipped and returned as entered
  NA, # NA values are skipped and returned as entered
  "a1b2c3d4e5f6g7h8i9j0", # Numerics should be removed
  "a'b-c d*e+f&g(h)i.j_k,", # Special characters should be removed
  "123" # Only non alphabetic characters supplied so should create blank string
)

# Correct names: Output that should be achieved by format function
NAME_RESULT <- c(
  "STEVEN", # "STEVEN",           # valid and formatted as expected
  "STEVEN", # "steven",           # requires converting to uppercase
  "JOHNPAUL", # "John-Paul",        # requires non alphabetic characters removed
  "JOHNPAUL", # "JOHN paul",        # requires non alphabetic characters removed
  "DAVIDJOHNPAUL", # "David John-Paul",  # requires non alphabetic characters removed
  "PETER", # "peter9", # requires non alphabetic characters removed
  "MCDONALD", # "Mc'Donald", # requires non alphabetic characters removed
  NA, # "", # Blank values are skipped and returned as entered
  NA, # NA, # NA values are skipped and returned as entered
  "ABCDEFGHIJ", # "a1b2c3d4e5f6g7h8i9j0",   # Numerics should be removed
  "ABCDEFGHIJK", # "a'b-c d*e+f&g(h)i.j_k,", # Special characters should be removed
  NA # "123" # Only non alphabetic characters supplied so should create blank string
)


# Save files to test folder
saveRDS(data.frame(ID = seq(1:length(NAME_TEST)), NAME = NAME_TEST), file = "./tests/testthat/testdata/test_name_input.rds")
saveRDS(data.frame(ID = seq(1:length(NAME_RESULT)), NAME = NAME_RESULT), file = "./tests/testthat/testdata/test_name_expected.rds")
