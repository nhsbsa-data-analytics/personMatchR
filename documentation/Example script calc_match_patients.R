# personMatchR example code: calc_match_patients() function
#
# created on 2022-10-25
#
# this code shows an example script running the matching process on flat file datasets


# Install and load required packages ----------------------------------------------------------
devtools::install_github("nhsbsa-data-analytics/personMatchR")
library("dplyr")
library("dbplyr")


# Import datasets for matching ----------------------------------------------------------------
df_a <- readRDS(url("https://github.com/nhsbsa-data-analytics/personMatchR/tree/main/R/documentation/TEST_DF_A.rds"))
df_b <- readRDS(url("https://github.com/nhsbsa-data-analytics/personMatchR/tree/main/R/documentation/TEST_DF_B.rds"))


# Review the test datasets --------------------------------------------------------------------
df_a
df_b


# Run the personMatchR package to identify matches --------------------------------------------
df_output <- personMatchR::calc_match_patients(
  df_one = df_a, # first dataset
  id_one = ID, # unique id field from first dataset
  forename_one = FORENAME, # forename field from first dataset
  surname_one = SURNAME, # surname field from first dataset
  dob_one = DOB, # date of birth field from first dataset
  postcode_one = POSTCODE, # postcode field from first dataset
  df_two = df_b, # second dataset
  id_two = ID, # unique id field from second dataset
  forename_two = FORENAME, # forename field from second dataset
  surname_two = SURNAME, # surname field from second dataset
  dob_two = DOB, # date of birth field from second dataset
  postcode_two = POSTCODE, # postcode field from second dataset
  output_type = "key", # only return the key match results
  format_data = TRUE, # format input datasets prior to matching
  inc_no_match = TRUE # return records from first dataset without matches
)


# Review the match output ---------------------------------------------------------------------
df_output
