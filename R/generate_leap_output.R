
# Part One: Libraries, Function and Connection ---------------------------------

# Define packages to install
packages_to_install = c("dplyr", "dbplyr", "tidytext", "purrr", "stringdist", "stringr", "DBI", "odbc")

# Install Packages
install.packages(packages_to_install)

# Library
library(dplyr)
library(dbplyr)

# Functions
source("R/format_name_db.R")
source("R/format_date_db.R")
source("R/format_postcode_db.R")
source("R/filter_name_db.R")
source("R/filter_dob_db.R")
source("R/calc_permutations_db.R")
source("R/calc_match_patients_db.R")

# Create Connection
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "Oracle in OraClient19Home1",
  DBQ = "",
  UID = "CYPHER",
  PWD = rstudioapi::askForPassword("Please enter your DALP password")
)

# Part Two: Pre-process tables -------------------------------------------------

# LEAP table
df_one <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("DALL_REF", "INT600_DWP_LEAP_FORMAT"))

# Other table (Your Data)
df_two <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("CYPHER", "MY_TABLE"))

# Process df_one (these are the LEAP column names)
df_one <- df_one %>%
  format_postcode_db(., POSTCODE) %>%
  format_name_db(., FORENAME) %>%
  format_name_db(., SURNAME) %>%
  format_date_db(., DATE_OF_BIRTH)

# Process df_two (Specify column names within functions as per your data)
df_two <- df_two %>%
  format_postcode_db(., POSTCODE) %>%
  format_name_db(., FORENAME) %>%
  format_name_db(., SURNAME) %>%
  format_date_db(., DOB)

# Write data back
df_one %>%
  compute(
    name = "INT600_LEAP_PROCESSED",
    temporary = FALSE
  )

# Write data back
df_two %>%
  compute(
    name = "INT600_MY_TABLE_PROCESSED",
    temporary = FALSE
  )

# Part Three: Generate Matching Output -----------------------------------------

# Proccesed LEAP table
df_one <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("CYPHER", "INT600_LEAP_PROCESSED"))

# Processed Other table
df_two <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("CYPHER", "INT600_MY_TABLE_PROCESSED"))

# Each df requires distinct names
# Leave LEAP column names as they are, rename df_two if necessary
df_two <- df_two %>%
  dplyr::rename(
    ID_TWO = ID, # EXAMPLE
    DOB_TWO = DOB, # EXAMPLE
    SURNAME_TWO = SURNAME, # EXAMPLE
    FORENAME_TWO = FORENAME, # EXAMPLE
    POSTCODE_TWO = POSTCODE # EXAMPLE
  )

# ENTER COLUMN NAMES AS PER YOUR DATASET NAMES
# Function output
results <- calc_match_patients_db(
  # Data to be matched
  df_one = df_one,
  id_one = ID,
  forename_one = FORENAME,
  surname_one = SURNAME,
  dob_one = DATE_OF_BIRTH,
  postcode_one = POSTCODE,
  # Lookup data
  df_two = df_two,
  id_two = ID_TWO,
  forename_two = FORENAME_TWO,
  surname_two = SURNAME_TWO,
  dob_two = DOB_TWO,
  postcode_two = POSTCODE_TWO,
  # Other Information
  output_type = "all",
  format_data = FALSE,
  inc_no_match = TRUE
)

# Write data back
results %>%
  compute(
    name = "INT600_MY_TABLE_NAME_RESULTS",
    temporary = FALSE
  )

# Part Four: Disconnect fro the Database ---------------------------------------

DBI::dbDisconnect(con)
