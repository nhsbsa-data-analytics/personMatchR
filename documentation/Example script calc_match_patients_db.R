# personMatchR example code: calc_match_patients_db() function
#
# created on 2022-10-25
#
# this code shows an example script running the matching process on database tables
# please note, to run this example you will need to amend database table references to schemas and
# tables which you have access to


# Install and load required packages ----------------------------------------------------------
devtools::install_github("nhsbsa-data-analytics/personMatchR")
devtools::install_github("nhsbsa-data-analytics/nhsbsaR")
library("dplyr")
library("dbplyr")
library("DBI")


# Establish connection to DALP database -------------------------------------------------------
# this can be changed to reflect whichever database connection is available

# using nhsbsaR package to connect to NHSBSA DALP database
con <- nhsbsaR::con_nhsbsa(database = "DALP")



# OPTIONAL: Copy test datasets to database environment ----------------------------------------
# this will copy the test datasets to the database environment as a test case example
DBI::dbWriteTable(con,
                  DBI::Id(schema = "STBUC", table = "PERSONMATCHR_INPUT_A"),
                  readRDS(url("https://github.com/nhsbsa-data-analytics/personMatchR/tree/main/R/documentation/TEST_DF_A.rds")),
                  field.types = c(
                    ID = "number(1,0)",
                    SURNAME = "varchar(26 byte)",
                    FORENAME = "varchar(26 byte)",
                    POSTCODE = "varchar(128 byte)",
                    DOB = "date"
                  ))

DBI::dbWriteTable(con,
                  DBI::Id(schema = "STBUC", table = "PERSONMATCHR_INPUT_B"),
                  readRDS(url("https://github.com/nhsbsa-data-analytics/personMatchR/tree/main/R/documentation/TEST_DF_B.rds")),
                  field.types = c(
                    ID = "number(1,0)",
                    SURNAME = "varchar(26 byte)",
                    FORENAME = "varchar(26 byte)",
                    POSTCODE = "varchar(128 byte)",
                    DOB = "date"
                  ))


# Import datasets from database ----------------------------------------------------------------
# establish connection to tables within the DALP database
df_a <- con |> dplyr::tbl(from = dbplyr::in_schema("STBUC", "PERSONMATCHR_INPUT_A"))
df_b <- con |> dplyr::tbl(from = dbplyr::in_schema("STBUC", "PERSONMATCHR_INPUT_B"))


# Review the test datasets --------------------------------------------------------------------
df_a |> dplyr::collect()
df_b |> dplyr::collect()


# Apply formatting functions to datasets ------------------------------------------------------
# it is highly recommended to apply the formatting functions to each dataset for the best matching
# please note, on large datasets these formatting functions may take a bit of time to run
# NB: field names may need amending to reflect relevant fields in database tables
df_a <- df_a |>
  personMatchR::format_postcode_db(POSTCODE) |>
  personMatchR::format_name_db(FORENAME) |>
  personMatchR::format_name_db(SURNAME) |>
  personMatchR::format_date_db(DOB)

df_b <- df_b |>
  personMatchR::format_postcode_db(POSTCODE) |>
  personMatchR::format_name_db(FORENAME) |>
  personMatchR::format_name_db(SURNAME) |>
  personMatchR::format_date_db(DOB)

# the formatted datasets should be written back to database to allow optimal performance
df_a |> dplyr::compute(name = "PERSONMATCHR_INPUT_A_FORMAT", temporary = FALSE)
df_b |> dplyr::compute(name = "PERSONMATCHR_INPUT_B_FORMAT", temporary = FALSE)


# Establish connection to the formatted tables ------------------------------------------------
df_a_fmt <- con |> dplyr::tbl(from = dbplyr::in_schema("STBUC", "PERSONMATCHR_INPUT_A_FORMAT"))
df_b_fmt <- con |> dplyr::tbl(from = dbplyr::in_schema("STBUC", "PERSONMATCHR_INPUT_B_FORMAT"))


# Review the formatting test datasets ---------------------------------------------------------
df_a_fmt |> dplyr::collect()
df_b_fmt |> dplyr::collect()


# Run the personMatchR package to identify matches --------------------------------------------
df_output <- personMatchR::calc_match_patients_db(
  df_one = df_a_fmt, # first dataset
  id_one = ID, # unique id field from first dataset
  forename_one = FORENAME, # forename field from first dataset
  surname_one = SURNAME, # surname field from first dataset
  dob_one = DOB, # date of birth field from first dataset
  postcode_one = POSTCODE, # postcode field from first dataset
  df_two = df_b_fmt, # second dataset
  id_two = ID, # unique id field from second dataset
  forename_two = FORENAME, # forename field from second dataset
  surname_two = SURNAME, # surname field from second dataset
  dob_two = DOB, # date of birth field from second dataset
  postcode_two = POSTCODE, # postcode field from second dataset
  output_type = "key", # only return the key match results
  inc_no_match = TRUE # return records from first dataset without matches
)

# the match results should be written back to database before being worked with
df_output |> dplyr::compute(name = "PERSONMATCHR_OUTPUT", temporary = FALSE)


# Review the match output ---------------------------------------------------------------------
con |>
  dplyr::tbl(from = dbplyr::in_schema("STBUC", "PERSONMATCHR_OUTPUT")) |>
  dplyr::collect()



# Close connection to DALP database -----------------------------------------------------------
DBI::dbDisconnect(con)
