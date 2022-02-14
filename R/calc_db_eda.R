
# Library
library(dplyr)
library(dbplyr)

# Functions
source("R/calc_db_functions.R")
source("R/format_db_functions.R")

#-------------------------------------------------------------------------------
# Part One: Table Formatting - Required prior due to multiple joins later

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

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
  format_db_postcode_simple(., POSTCODE) %>%
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

# Exact matches
exact <- eib_db %>%
  select(REFERENCE, FORENAME, SURNAME, DOB, POSTCODE) %>%
  inner_join(
    y = pds_db %>% select(RECORD_ID, FORENAME, SURNAME, DOB, POSTCODE)
  ) %>%
  distinct() %>%
  mutate(
    DOB_EIB = DOB,
    SURNAME_EIB = SURNAME,
    FORENAME_EIB = FORENAME,
    POSTCODE_EIB = POSTCODE,
    DOB_PDS = DOB,
    SURNAME_PDS = SURNAME,
    FORENAME_PDS = FORENAME,
    POSTCODE_PDS = POSTCODE,
    MATCH_TYPE = 'Exact'
  ) %>%
  select(
    REFERENCE,
    RECORD_ID,
    DOB_EIB,
    SURNAME_EIB,
    FORENAME_EIB,
    POSTCODE_EIB,
    DOB_PDS = DOB,
    SURNAME_PDS,
    FORENAME_PDS,
    POSTCODE_PDS,
    MATCH_TYPE
  )

# Remaining records
eib_db <- eib_db %>%
  anti_join(y = exact, by = "REFERENCE")
eib_db
# Permutation join
perm_join <- function(df1, df2, id_one, id_two, perm_num){

  output <- inner_join(
    x = df1 %>% select({{ id_one}}, {{ perm_num }}),
    y = df2 %>% select({{ id_two}}, {{ perm_num }})
  ) %>%
    select(-{{ perm_num }})
  return(output)
}

# Get list of lookup pairs
id_pairs <- perm_join(eib_db, pds_db, REFERENCE, RECORD_ID, PERM1) %>%
  union_all(
    perm_join(eib_db, pds_db, REFERENCE, RECORD_ID, PERM2)
  ) %>%
  union_all(
    perm_join(eib_db, pds_db, REFERENCE, RECORD_ID, PERM3)
  ) %>%
  union_all(
    perm_join(eib_db, pds_db, REFERENCE, RECORD_ID, PERM4)
    ) %>%
  union_all(
    perm_join(eib_db, pds_db, REFERENCE, RECORD_ID, PERM5)
  ) %>%
  union_all(
    perm_join(eib_db, pds_db, REFERENCE, RECORD_ID, PERM6)
  ) %>%
  union_all(
    perm_join(eib_db, pds_db, REFERENCE, RECORD_ID, PERM7)
  ) %>%
  union_all(
    perm_join(eib_db, pds_db, REFERENCE, RECORD_ID, PERM8)
  ) %>%
  union_all(
    perm_join(eib_db, pds_db, REFERENCE, RECORD_ID, PERM9)
  ) %>%
  select(REFERENCE, RECORD_ID) %>%
  distinct()

# Join lookup back to original dfs
output <- id_pairs %>%
  inner_join(
    y = eib_db %>% select(
      REFERENCE,
      DOB_EIB = DOB,
      SURNAME_EIB = SURNAME,
      FORENAME_EIB = FORENAME,
      POSTCODE_EIB = POSTCODE
      ),
    by = "REFERENCE"
  ) %>%
  inner_join(
    y = pds_db %>% select(
      RECORD_ID,
      DOB_PDS = DOB,
      SURNAME_PDS = SURNAME,
      FORENAME_PDS = FORENAME,
      POSTCODE_PDS = POSTCODE
    ),
    by = "RECORD_ID"
  )

# Generate list of feasible dob-pairs with 6 identical characters
eligible_dates <- dob_lv_filter(output, DOB_EIB, DOB_PDS)

# Generate a list of feasible forename pairs
eligible_names <- calc_db_jw_threshold(output, FORENAME_EIB, FORENAME_PDS, 0.75, 'JW_FORENAME')

# Filter permutation output by feasible date-pair list
output_filter <- output %>%
  inner_join(eligible_dates) %>%
  inner_join(eligible_names)

# Generate a list
match <- output_filter %>%
  mutate(
    JW_SURNAME = UTL_MATCH.JARO_WINKLER(SURNAME_EIB, SURNAME_PDS),
    JW_POSTCODE = UTL_MATCH.JARO_WINKLER(POSTCODE_EIB, POSTCODE_PDS),
    ED_DOB = ifelse(DOB_EIB == DOB_PDS, 0, 2)
  ) %>%
  # limit to key fields and score matches
  dplyr::mutate(MATCH_TYPE = case_when(
    (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB == 0) ~ "Exact",
    (JW_SURNAME == 1 & JW_FORENAME == 1 & ED_DOB == 0) ~ "Confident",
    (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB <= 2) ~ "Confident",
    (JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB == 0) ~ "Confident",
    (JW_SURNAME == 1 & JW_FORENAME >= 0.75 & JW_POSTCODE == 1 & ED_DOB == 0) ~ "Confident",
    (JW_SURNAME >= 0.85 & JW_FORENAME >= 0.75 & JW_POSTCODE >= 0.85 & ED_DOB <= 2) ~ "Confident",
    TRUE ~ "No Match"
    )
  ) %>%
  # filter to only confident matches
  dplyr::filter(MATCH_TYPE != "No Match")

match

all_match <- match %>%
  dplyr::union_all(exact) %>%
  dplyr::group_by(REFERENCE) %>%
  dplyr::mutate(MATCH_COUNT = n()) %>%
  dplyr::ungroup()

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
