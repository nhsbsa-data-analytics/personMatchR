#' Test cases for format_postcode function
#' Test dataset creation
#'
#' BUild the dataframes for the test cases
#'


# Test Case Dataset ---------------------------------------------------------------------------

# Test postcodes: attempt to cover all scenarios
POSTCODE_TEST <- c(
  # 5 character postcodes (A99AA)
  "A9 9AA", # 5 digit postcode : valid
  "A99AA", # 5 digit postcode : missing space
  "A9-9AA", # 5 digit postcode : including hyphen
  "A9   9AA", # 5 digit postcode : multiple spaces
  "A 99 AA", # 5 digit postcode : unconventional formatting
  "a9 9aA", # 5 digit postcode : mixed cases
  "09 900", # 5 digit postcode : 0 instead of O in character location
  "59 955", # 5 digit postcode : 5 instead of S in character location
  "AI IAA", # 5 digit postcode : I instead of 1 in numeric location
  "Al lAA", # 5 digit postcode : L instead of 1 in numeric location
  "Ao oAA", # 5 digit postcode : O instead of 0 in numeric location
  "A5 5AA", # 5 digit postcode : S instead of 5 in numeric location
  # 6 character postcodes (A9A9AA / A999AA / AA99AA)
  "A9A 9AA", # 6 digit postcode : valid
  "A99 9AA", # 6 digit postcode : valid
  "AA9 9AA", # 6 digit postcode : valid
  "A9A9AA", # 6 digit postcode : missing space
  "A9A-9AA", # 6 digit postcode : including hyphen
  "A9A   9AA", # 6 digit postcode : multiple spaces
  "A 9A9 AA", # 6 digit postcode : unconventional formatting
  "a9a 9aA", # 6 digit postcode : mixed cases
  "099 900", # 6 digit postcode : 0 instead of O in character location
  "599 955", # 6 digit postcode : 5 instead of S in character location
  "AA9 IAA", # 6 digit postcode : I instead of 1 in numeric location
  "AA9 lAA", # 6 digit postcode : L instead of 1 in numeric location
  "AA9 oAA", # 6 digit postcode : O instead of 0 in numeric location
  "AA9 5AA", # 6 digit postcode : S instead of 5 in numeric location
  # 7 character postcodes (AA9A9AA / AA999AA)
  "AA9A 9AA", # 7 digit postcode : valid
  "AA99 9AA", # 7 digit postcode : valid
  "AA9A9AA", # 7 digit postcode : missing space
  "AA9A-9AA", # 7 digit postcode : including hyphen
  "AA9A   9AA", # 7 digit postcode : multiple spaces
  "AA 9A 9A A", # 7 digit postcode : unconventional formatting
  "Aa9A 9aA", # 7 digit postcode : mixed cases
  "0099 900", # 7 digit postcode : 0 instead of O in character location
  "5599 955", # 7 digit postcode : 5 instead of S in character location
  "AAIA IAA", # 7 digit postcode : I instead of 1 in numeric location
  "AAlA lAA", # 7 digit postcode : L instead of 1 in numeric location
  "AAoA oAA", # 7 digit postcode : O instead of 0 in numeric location
  "AA5A 5AA", # 7 digit postcode : S instead of 5 in numeric location
  "5A5A 5AA", # 7 digit postcode : S instead of 5 in numeric location
  "505A5AA", # 7 digit postcode : S instead of 5 in numeric location 1 and 0 instead of O in pos.2
  # misc
  NA, # NA value
  "", # Empty text string
  "No Postcode", # Non postcode text string
  "AA99A 9AA" # Additional characters
)

# Correct postcodes: Output that should be achieved by format function
POSTCODE_RESULT <- c(
  # 5 character postcodes (A99AA)
  "A99AA", # "A9 9AA", # 5 digit postcode : valid
  "A99AA", # "A99AA", # 5 digit postcode : missing space
  "A99AA", # "A9-9AA", # 5 digit postcode : including hyphen
  "A99AA", # "A9   9AA", # 5 digit postcode : multiple spaces
  "A99AA", # "A 99 AA", # 5 digit postcode : unconventional formatting
  "A99AA", # "a9 9aA", # 5 digit postcode : mixed cases
  "O99OO", # "09 900", # 5 digit postcode : 0 instead of O in character location
  "S99SS", # "59 955", # 5 digit postcode : 5 instead of S in character location
  "A11AA", # "AI IAA", # 5 digit postcode : I instead of 1 in numeric location
  "A11AA", # "Al lAA", # 5 digit postcode : L instead of 1 in numeric location
  "A00AA", # "Ao oAA", # 5 digit postcode : O instead of 0 in numeric location
  "A55AA", # "A5 5AA", # 5 digit postcode : S instead of 5 in numeric location
  # 6 character postcodes (A9A9AA / A999AA / AA99AA)
  "A9A9AA", # "A9A 9AA", # 6 digit postcode : valid
  "A999AA", # "A99 9AA", # 6 digit postcode : valid
  "AA99AA", # "AA9 9AA", # 6 digit postcode : valid
  "A9A9AA", # "A9A9AA", # 6 digit postcode : missing space
  "A9A9AA", # "A9A-9AA", # 6 digit postcode : including hyphen
  "A9A9AA", # "A9A   9AA", # 6 digit postcode : multiple spaces
  "A9A9AA", # "A 9A9 AA", # 6 digit postcode : unconventional formatting
  "A9A9AA", # "a9a 9aA", # 6 digit postcode : mixed cases
  "O999OO", # "099 900", # 6 digit postcode : 0 instead of O in character location
  "S999SS", # "599 955", # 6 digit postcode : 5 instead of S in character location
  "AA91AA", # "AA9 IAA", # 6 digit postcode : I instead of 1 in numeric location
  "AA91AA", # "AA9 lAA", # 6 digit postcode : L instead of 1 in numeric location
  "AA90AA", # "AA9 oAA", # 6 digit postcode : O instead of 0 in numeric location
  "AA95AA", # "AA9 5AA", # 6 digit postcode : S instead of 5 in numeric location
  # 7 character postcodes (AA9A9AA / AA999AA)
  "AA9A9AA", # "AA9A 9AA", # 7 digit postcode : valid
  "AA999AA", # "AA99 9AA", # 7 digit postcode : valid
  "AA9A9AA", # "AA9A9AA", # 7 digit postcode : missing space
  "AA9A9AA", # "AA9A-9AA", # 7 digit postcode : including hyphen
  "AA9A9AA", # "AA9A   9AA", # 7 digit postcode : multiple spaces
  "AA9A9AA", # "AA 9A 9A A", # 7 digit postcode : unconventional formatting
  "AA9A9AA", # "Aa9A 9aA", # 7 digit postcode : mixed cases
  "OO999OO", # "0099 900", # 7 digit postcode : 0 instead of O in character location
  "SS999SS", # "5599 955", # 7 digit postcode : 5 instead of S in character location
  "AA1A1AA", # "AAIA IAA", # 7 digit postcode : I instead of 1 in numeric location
  "AA1A1AA", # "AAlA lAA", # 7 digit postcode : L instead of 1 in numeric location
  "AA0A0AA", # "AAoA oAA", # 7 digit postcode : O instead of 0 in numeric location
  "AA5A5AA", # "AA5A 5AA", # 7 digit postcode : S instead of 5 in numeric location
  "SA5A5AA", # 7 digit postcode : S instead of 5 in numeric location
  "SO5A5AA", # 7 digit postcode : S instead of 5 in numeric location 1 and 0 instead of O in pos.2
  # misc
  NA, # NA, # NA value
  NA, # "", # Empty text string
  "NOPOSTCODE", # "MISSING", # Non postcode text string
  "AA99A9AA" # "AA99A 9AA" # Additional characters
)


# Save files to test folder
saveRDS(data.frame(ID = seq(1:length(POSTCODE_TEST)), POSTCODE = POSTCODE_TEST), file = "./tests/testthat/testdata/test_postcode_input.rds")
saveRDS(data.frame(ID = seq(1:length(POSTCODE_RESULT)), POSTCODE = POSTCODE_RESULT), file = "./tests/testthat/testdata/test_postcode_expected.rds")
