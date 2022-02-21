
# Library
library(dplyr)
library(dbplyr)

# Functions
source("R/calc_db_functions.R")
source("R/format_db_functions.R")

#-------------------------------------------------------------------------------
# Part One: Table Formatting - Required prior due to multiple joins later

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP", dsn = NULL)

# Db pds table
pds_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("STBUC", "INT617_TMP_PDS"))

# Db eibss table
eib_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("STBUC", "INT617_TMP_EIBSS"))

# Format EIBBS data
eib <- eib_db %>%
  select(REFERENCE, DOB, SURNAME, FORENAME, POSTCODE) %>%
  format_db_postcode(., POSTCODE) %>%
  format_db_name(., FORENAME) %>%
  format_db_name(., SURNAME) %>%
  format_db_date(., DOB) %>%
  calc_permutations(., FORENAME, SURNAME, POSTCODE, DOB)

# Format PDS data
pds <-pds_db %>%
  select(RECORD_ID, DOB, SURNAME, FORENAME, POSTCODE) %>%
  format_db_postcode(., POSTCODE) %>%
  format_db_name(., FORENAME) %>%
  format_db_name(., SURNAME) %>%
  format_db_date(., DOB) %>%
  calc_permutations(., FORENAME, SURNAME, POSTCODE, DOB)

# Write the table back to the DB: 1 min
eib %>%
  compute(
    name = "INT617_EIB_PROCESSED",
    temporary = FALSE
  )

# Write the table back to the DB: 35 mins
pds %>%
  compute(
    name = "INT617_PDS_PROCESSED",
    temporary = FALSE
  )

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
# Part Two: composite joins then scoring

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db pds table
eib_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT617_EIB_PROCESSED"))

# Db eibss table
pds_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT617_PDS_PROCESSED"))

# Rename PDS relevant variables
pds_db <- pds_db %>%
  rename(
    DOB_PDS = DOB,
    SURNAME_PDS = SURNAME,
    FORENAME_PDS = FORENAME,
    POSTCODE_PDS = POSTCODE
  )

# Results
results <- find_db_matches(
  eib_db, REFERENCE, FORENAME, SURNAME, DOB, POSTCODE,
  pds_db, RECORD_ID, FORENAME_PDS, SURNAME_PDS, DOB_PDS, POSTCODE_PDS
)

# Print results
results

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
