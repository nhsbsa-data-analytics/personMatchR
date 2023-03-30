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
  "AA99A 9AA", # Additional characters
  "POSTCODE", # Non postcode text string
  "Address postcode", # Non postcode text string
  "NE15 8NY", # Valid postcode
  "Ne15-8ny", # Valid postcode - mixed case - special characters
  "**Ne15-8ny**", # Valid postcode - mixed case - special characters
  "AA9A 9AA", # Homoglyph issue - 7 characters - valid
  "AA99 9AA", # Homoglyph issue - 7 characters - valid
  "5E15 8NY", # Homoglyph issue - 7 characters - position 1 - 5 instead of S
  "0E15 0NY", # Homoglyph issue - 7 characters - position 1 - 0 instead of O
  "N515 8NY", # Homoglyph issue - 7 characters - position 2 - 5 instead of S
  "N015 0NY", # Homoglyph issue - 7 characters - position 2 - 0 instead of O
  "NES5 0NY", # Homoglyph issue - 7 characters - position 3 - S instead of 5
  "NEo5 0NY", # Homoglyph issue - 7 characters - position 3 - O instead of 0
  "NEi5 0NY", # Homoglyph issue - 7 characters - position 3 - i instead of 1
  "NEL5 0NY", # Homoglyph issue - 7 characters - position 3 - L instead of 1
  "NE15 SNY", # Homoglyph issue - 7 characters - position 5 - S instead of 5
  "NE15 oNY", # Homoglyph issue - 7 characters - position 5 - O instead of 0
  "NE15 INY", # Homoglyph issue - 7 characters - position 5 - i instead of 1
  "NE15 lny", # Homoglyph issue - 7 characters - position 5 - L instead of 1
  "NE15 85Y", # Homoglyph issue - 7 characters - position 6 - 5 instead of S
  "NE15 80Y", # Homoglyph issue - 7 characters - position 6 - 0 instead of O
  "NE15 8N5", # Homoglyph issue - 7 characters - position 7- 5 instead of S
  "NE15 8N0", # Homoglyph issue - 7 characters - position 7 - 0 instead of O
  "50SA i50", # Homoglyph issue - 7 characters - all positions
  "A9A 9AA", # Homoglyph issue - 6 characters - valid
  "A99 9AA", # Homoglyph issue - 6 characters - valid
  "AA9 9AA", # Homoglyph issue - 6 characters - valid
  "5A9 5AA", # Homoglyph issue - 6 characters - position 1 - 5 instead of S
  "0A9 0AA", # Homoglyph issue - 6 characters - position 1 - 0 instead of O
  "A99 sAA", # Homoglyph issue - 6 characters - position 4 - S instead of 5
  "A99 oAA", # Homoglyph issue - 6 characters - position 4 - O instead of 0
  "A99 iAA", # Homoglyph issue - 6 characters - position 4 - i instead of 1
  "A99 LAA", # Homoglyph issue - 6 characters - position 4 - L instead of 1
  "A99 95A", # Homoglyph issue - 6 characters - position 5 - 5 instead of S
  "A99 90A", # Homoglyph issue - 6 characters - position 5 - 0 instead of O
  "A99 9A5", # Homoglyph issue - 6 characters - position 6 - 5 instead of S
  "A99 9A0", # Homoglyph issue - 6 characters - position 6 - 0 instead of O
  "5A9 o50", # Homoglyph issue - 6 characters - all positions
  "A9 9AA", # Homoglyph issue - 5 characters - valid
  "59 9AA", # Homoglyph issue - 5 characters - position 1 - 5 instead of S
  "09 9AA", # Homoglyph issue - 5 characters - position 1 - 0 instead of O
  "AS 9AA", # Homoglyph issue - 5 characters - position 2 - S instead of 5
  "Ao 9AA", # Homoglyph issue - 5 characters - position 2 - O instead of 0
  "Ai 9AA", # Homoglyph issue - 5 characters - position 2 - i instead of 1
  "AL 9AA", # Homoglyph issue - 5 characters - position 2 - L instead of 1
  "A9 SAA", # Homoglyph issue - 5 characters - position 3 - S instead of 5
  "A9 oAA", # Homoglyph issue - 5 characters - position 3 - O instead of 0
  "A9 iaa", # Homoglyph issue - 5 characters - position 3 - i instead of 1
  "A9 Laa", # Homoglyph issue - 5 characters - position 3 - L instead of 1
  "A9 95A", # Homoglyph issue - 5 characters - position 4 - 5 instead of S
  "A9 90A", # Homoglyph issue - 5 characters - position 4 - 0 instead of O
  "A9 9A5", # Homoglyph issue - 5 characters - position 5 - 5 instead of S
  "A9 9A0", # Homoglyph issue - 5 characters - position 5 - 0 instead of O
  "5o i50" # Homoglyph issue - 5 characters - all positions
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
  "AA99A9AA", # "AA99A 9AA" # Additional characters
  "POSTCODE", # Non postcode text string
  "ADDRESSPOSTCODE", # Non postcode text string
  "NE158NY", # Valid postcode
  "NE158NY", # Valid postcode - mixed case - special characters
  "NE158NY", # Valid postcode - mixed case - special characters
  "AA9A9AA", # Homoglyph issue - 7 characters - valid
  "AA999AA", # Homoglyph issue - 7 characters - valid
  "SE158NY", # Homoglyph issue - 7 characters - position 1 - 5 instead of S
  "OE150NY", # Homoglyph issue - 7 characters - position 1 - 0 instead of O
  "NS158NY", # Homoglyph issue - 7 characters - position 2 - 5 instead of S
  "NO150NY", # Homoglyph issue - 7 characters - position 2 - 0 instead of O
  "NE550NY", # Homoglyph issue - 7 characters - position 3 - S instead of 5
  "NE050NY", # Homoglyph issue - 7 characters - position 3 - O instead of 0
  "NE150NY", # Homoglyph issue - 7 characters - position 3 - i instead of 1
  "NE150NY", # Homoglyph issue - 7 characters - position 3 - L instead of 1
  "NE155NY", # Homoglyph issue - 7 characters - position 5 - S instead of 5
  "NE150NY", # Homoglyph issue - 7 characters - position 5 - O instead of 0
  "NE151NY", # Homoglyph issue - 7 characters - position 5 - i instead of 1
  "NE151NY", # Homoglyph issue - 7 characters - position 5 - L instead of 1
  "NE158SY", # Homoglyph issue - 7 characters - position 6 - 5 instead of S
  "NE158OY", # Homoglyph issue - 7 characters - position 6 - 0 instead of O
  "NE158NS", # Homoglyph issue - 7 characters - position 7- 5 instead of S
  "NE158NO", # Homoglyph issue - 7 characters - position 7 - 0 instead of O
  "SO5A1SO", # Homoglyph issue - 7 characters - all positions
  "A9A9AA", # Homoglyph issue - 6 characters - valid
  "A999AA", # Homoglyph issue - 6 characters - valid
  "AA99AA", # Homoglyph issue - 6 characters - valid
  "SA95AA", # Homoglyph issue - 6 characters - position 1 - 5 instead of S
  "OA90AA", # Homoglyph issue - 6 characters - position 1 - 0 instead of O
  "A995AA", # Homoglyph issue - 6 characters - position 4 - S instead of 5
  "A990AA", # Homoglyph issue - 6 characters - position 4 - O instead of 0
  "A991AA", # Homoglyph issue - 6 characters - position 4 - i instead of 1
  "A991AA", # Homoglyph issue - 6 characters - position 4 - L instead of 1
  "A999SA", # Homoglyph issue - 6 characters - position 5 - 5 instead of S
  "A999OA", # Homoglyph issue - 6 characters - position 5 - 0 instead of O
  "A999AS", # Homoglyph issue - 6 characters - position 6 - 5 instead of S
  "A999AO", # Homoglyph issue - 6 characters - position 6 - 0 instead of O
  "SA90SO", # Homoglyph issue - 6 characters - all positions
  "A99AA", # Homoglyph issue - 5 characters - valid
  "S99AA", # Homoglyph issue - 5 characters - position 1 - 5 instead of S
  "O99AA", # Homoglyph issue - 5 characters - position 1 - 0 instead of O
  "A59AA", # Homoglyph issue - 5 characters - position 2 - S instead of 5
  "A09AA", # Homoglyph issue - 5 characters - position 2 - O instead of 0
  "A19AA", # Homoglyph issue - 5 characters - position 2 - i instead of 1
  "A19AA", # Homoglyph issue - 5 characters - position 2 - L instead of 1
  "A95AA", # Homoglyph issue - 5 characters - position 3 - S instead of 5
  "A90AA", # Homoglyph issue - 5 characters - position 3 - O instead of 0
  "A91AA", # Homoglyph issue - 5 characters - position 3 - i instead of 1
  "A91AA", # Homoglyph issue - 5 characters - position 3 - L instead of 1
  "A99SA", # Homoglyph issue - 5 characters - position 4 - 5 instead of S
  "A99OA", # Homoglyph issue - 5 characters - position 4 - 0 instead of O
  "A99AS", # Homoglyph issue - 5 characters - position 5 - 5 instead of S
  "A99AO", # Homoglyph issue - 5 characters - position 5 - 0 instead of O
  "S01SO" # Homoglyph issue - 5 characters - all positions
)


# Save files to test folder
test_postcode_input <- data.frame(ID = seq(1:length(POSTCODE_TEST)), POSTCODE = POSTCODE_TEST)
save(test_postcode_input, file = "./tests/testthat/testdata/test_postcode_input.rda")
test_postcode_expected <- data.frame(ID = seq(1:length(POSTCODE_RESULT)), POSTCODE = POSTCODE_RESULT)
save(test_postcode_expected, file = "./tests/testthat/testdata/test_postcode_expected.rda")
