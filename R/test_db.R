
#-------------------------------------------------------------------------------
# Patient Matching Code Example

# Install patientMatchR Package
install.packages("devtools")
devtools::install_github("nhsbsa-data-analytics/PatientMatchR")

devtools::install_github("nhsbsa-data-analytics/PersonMatchR")

# Library
library(dplyr)
library(dbplyr)
library(patientMatchR)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db pds table
df_one <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "PATIENT_MATCHING_DF_ONE"))

# Db eibss table
df_two <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "PATIENT_MATCHING_DF_TWO"))

# Function output
results <- calc_match_patients_db(
  # Data to be matched
  df_one = df_one,
  id_one = REFERENCE,
  forename_one = FORENAME,
  surname_one = SURNAME,
  dob_one = DOB,
  postcode_one = POSTCODE,
  # Lookup data
  df_two = df_two,
  id_two = ID_PDS,
  forename_two = FORENAME_PDS,
  surname_two = SURNAME_PDS,
  dob_two = DOB_PDS,
  postcode_two = POSTCODE_PDS,
  # Other Information
  output_type = "all",
  format_data = TRUE
)

# Write data back
results %>%
  compute(
    name = "PATIENT_MATCHING_RESULTS",
    temporary = FALSE
  )

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
# Patient Matching Code Example

# Install patientMatchR Package
install.packages("devtools")
devtools::install_github("nhsbsa-data-analytics/patieentMatchR")

# Library
library(dplyr)
library(dbplyr)

# Functions
source("R/format_name_db.R")
source("R/format_date_db.R")
source("R/format_date_parse_db.R")
source("R/format_postcode_db.R")
source("R/filter_name_db.R")
source("R/filter_dob_db.R")
source("R/calc_permutations_db.R")
source("R/calc_match_patients_db.R")

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db pds table
eib_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT617_EIB_PROCESSED"))

# Db eibss table
pds_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_PDS_PROCESSED"))

# Format EIBSS data
eib_db <- eib_db %>%
  select(REFERENCE, DOB, FORENAME, SURNAME, POSTCODE)

# Rename PDS relevant variables
pds_db <- pds_db %>%
  rename(
    ID_PDS = RECORD_ID,
    DOB_PDS = DOB,
    SURNAME_PDS = SURNAME,
    FORENAME_PDS = FORENAME,
    POSTCODE_PDS = POSTCODE
  )

# Check formatting
eib_db %>%
  format_postcode_db(., POSTCODE) %>%
  format_name_db(., FORENAME) %>%
  format_name_db(., SURNAME) %>%
  format_date_db(., DOB) %>%
  show_query()

# Function output
results <- calc_match_patients_db(
  # Data to be matched
  df_one = eib_db,
  id_one = REFERENCE,
  forename_one = FORENAME,
  surname_one = SURNAME,
  dob_one = DOB,
  postcode_one = POSTCODE,
  # Lookup data
  df_two = pds_db,
  id_two = ID_PDS,
  forename_two = FORENAME_PDS,
  surname_two = SURNAME_PDS,
  dob_two = DOB_PDS,
  postcode_two = POSTCODE_PDS,
  # Other Information
  output_type = "all",
  format_data = FALSE,
  inc_no_match = TRUE
  )

# Function output
results2 <- calc_match_patients_db(
  # Data to be matched
  df_one = eib_db,
  id_one = REFERENCE,
  forename_one = FORENAME,
  surname_one = SURNAME,
  dob_one = DOB,
  postcode_one = POSTCODE,
  # Lookup data
  df_two = pds_db,
  id_two = ID_PDS,
  forename_two = FORENAME_PDS,
  surname_two = SURNAME_PDS,
  dob_two = DOB_PDS,
  postcode_two = POSTCODE_PDS,
  # Other Information
  output_type = "all",
  format_data = FALSE,
  inc_no_match = TRUE
  )

Sys.time()
# Write data back
results %>%
  compute(
    name = "PATIENT_MATCHING_RESULTS",
    temporary = FALSE
  )
Sys.time()

# Write data back
results2 %>%
  compute(
    name = "PATIENT_MATCHING_RESULTS2",
    temporary = FALSE
  )
Sys.time()

# Disconnect
DBI::dbDisconnect()


#-------------------------------------------------------------------------------
# Part Three: Manual Code Run

# Check data
df_one <- eib_db %>%
  rename(
    ID_ONE = REFERENCE,
    DOB_ONE = DOB,
    SURNAME_ONE = SURNAME,
    FORENAME_ONE = FORENAME,
    POSTCODE_ONE = POSTCODE
  )

# Rename PDS Fields
df_two <- pds_db %>%
  rename(
    ID_TWO = ID_PDS,
    DOB_TWO = DOB_PDS,
    SURNAME_TWO = SURNAME_PDS,
    FORENAME_TWO = FORENAME_PDS,
    POSTCODE_TWO = POSTCODE_PDS
  )

# Df column names
df_one_cols <- colnames(df_one)
df_two_cols <- colnames(df_two)

# Exact matches
exact_matches <- df_one %>%
  dplyr::inner_join(
    y = df_two,
    by = c(
      "FORENAME_ONE" = "FORENAME_TWO",
      "SURNAME_ONE" = "SURNAME_TWO",
      "DOB_ONE" = "DOB_TWO",
      "POSTCODE_ONE" = "POSTCODE_TWO"
    )
  )

# Reverse exact matches
exact_matches_reverse <- df_one %>%
  dplyr::inner_join(
    y = df_two,
    by = c(
      "FORENAME_ONE" = "SURNAME_TWO",
      "SURNAME_ONE" = "FORENAME_TWO",
      "DOB_ONE" = "DOB_TWO",
      "POSTCODE_ONE" = "POSTCODE_TWO"
    )
  )

# Union exact matches
exact_matches <- exact_matches %>%
  dplyr::union_all(exact_matches_reverse) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    FORENAME_TWO = FORENAME_ONE,
    SURNAME_TWO = SURNAME_ONE,
    DOB_TWO = DOB_ONE,
    POSTCODE_TWO = POSTCODE_ONE,
    JW_FORENAME = 1,
    JW_SURNAME = 1,
    JW_POSTCODE = 1,
    ED_DOB = 0,
    MATCH_TYPE = 'Exact',
  )

# Remaining records
remain <- df_one %>%
  dplyr::anti_join(y = exact_matches, by = "ID_ONE") %>%
  calc_permutations_db(., FORENAME_ONE, SURNAME_ONE, POSTCODE_ONE, DOB_ONE)

remain %>% tally()

# Select columns and calculate permutations
df_two <- df_two %>%
  calc_permutations_db(., FORENAME_TWO, SURNAME_TWO, POSTCODE_TWO, DOB_TWO)

# List of permutation-join columns
perm_num <- paste0("PERM", 1:9)

# Distinct list of ID perm-join pairs
id_pairs <- perm_num %>%
  purrr::map(~{

    remain %>%
      dplyr::select(all_of(df_one_cols), {{.x}}) %>%
      dplyr::inner_join(
        df_two %>%
          dplyr::select(all_of(df_two_cols), {{.x}}),
        by = {{.x}}
      ) %>%
      dplyr::select(- {{.x}})
  }) %>%
  purrr::reduce(function(x, y) union(x, y)) %>%
  dplyr::distinct()

id_pairs %>% tally()

# Generate list of feasible dob-pairs with 6 identical characters
cross <- id_pairs %>%
  name_db_filter(., FORENAME_ONE, FORENAME_TWO) %>%
  dob_db_filter(., DOB_ONE, DOB_TWO, 2)

# Generate a list
matches <- cross %>%
  dplyr::mutate(
    # If single character forename handle differently, otherwise JW
    JW_FORENAME = dplyr::case_when(
      length(FORENAME_ONE) == 1 & FORENAME_ONE == substr(FORENAME_TWO, 1, 1) ~ 0.75,
      FORENAME_ONE == FORENAME_TWO ~ 1,
      T ~ UTL_MATCH.JARO_WINKLER(FORENAME_ONE, FORENAME_TWO)
    )
  ) %>%
  dplyr::filter(JW_FORENAME >= 0.75) %>%
  dplyr::mutate(
    # JW match, bypassing exact string matches (DIFF_DOB already calculated)
    JW_SURNAME = ifelse(SURNAME_ONE == SURNAME_TWO, 1, UTL_MATCH.JARO_WINKLER(SURNAME_ONE, SURNAME_TWO)),
    JW_POSTCODE = ifelse(POSTCODE_ONE == POSTCODE_TWO, 1, UTL_MATCH.JARO_WINKLER(POSTCODE_ONE, POSTCODE_TWO)),
    # Generate confident matches
    MATCH_TYPE = dplyr::case_when(
      (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DIFF_DOB == 0) ~ "Exact",
      (JW_SURNAME == 1 & JW_FORENAME == 1 & DIFF_DOB == 0) ~ "Confident",
      (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DIFF_DOB <= 2) ~ "Confident",
      (JW_FORENAME == 1 & JW_POSTCODE == 1 & DIFF_DOB == 0) ~ "Confident",
      (JW_SURNAME == 1 & JW_FORENAME >= 0.75 & JW_POSTCODE == 1 & DIFF_DOB == 0) ~ "Confident",
      (JW_SURNAME >= 0.85 & JW_FORENAME >= 0.75 & JW_POSTCODE >= 0.85 & DIFF_DOB <= 2) ~ "Confident",
      TRUE ~ "No Match"
    )
  ) %>%
  # filter to only confident matches
  dplyr::filter(MATCH_TYPE != "No Match") %>%
  # Add exact matches
  dplyr::union_all(exact_matches)

# Determine missing non-match fields
non_matches <- df_one %>%
  dplyr::anti_join(y = matches %>% dplyr::select(ID_ONE)) %>%
  dplyr::mutate(
    ID_TWO = NA,
    FORENAME_TWO = NA,
    SURNAME_TWO= NA,
    DOB_TWO = NA,
    POSTCODE_TWO = NA,
    JW_SURNAME = NA,
    JW_FORENAME = NA,
    JW_POSTCODE = NA,
    ED_DOB = NA,
    MATCH_TYPE = "No Match"
  )

# Add non-matches
all_matches <- matches %>%
  dplyr::union_all(non_matches) %>%
  # Calculate match_count per primary df ID
  dplyr::group_by(ID_ONE) %>%
  dplyr::mutate(MATCH_COUNT = dplyr::n_distinct(ID_TWO)) %>%
  dplyr::ungroup()

#-------------------------------------------------------------------------------
# Part 4.1: Create date-pairs list

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db pds table
res <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_EIBSS_TEST2"))

# Db eibss table
pds_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_PDS_PROCESSED"))

# Rename RES fields
res <- res %>%
  rename(
    ID_ONE = REFERENCE,
    DOB_ONE = DOB,
    SURNAME_ONE = SURNAME,
    FORENAME_ONE = FORENAME,
    POSTCODE_ONE = POSTCODE
  ) %>%
  filter(MATCH_TYPE == "No Match")

# Rename PDS Fields
pds_db <- pds_db %>%
  rename(
    ID_TWO = RECORD_ID,
    DOB_TWO = DOB,
    SURNAME_TWO = SURNAME,
    FORENAME_TWO = FORENAME,
    POSTCODE_TWO = POSTCODE
  )

# List of eligible date pairs: 130k
date_pairs <- pds_db %>%
  select(DOB_TWO) %>%
  distinct() %>%
  full_join(
    res %>%
      select(DOB_ONE) %>%
      distinct(),
    by = character()
  ) %>%
  dob_db_filter(., DOB_ONE, DOB_TWO, 2)

# Compute date pairs
date_pairs %>%
  compute(
    name = "INT600_EIBSS_DATE_PAIRS",
    temporary = FALSE
  )

# Part 4.2 ---------------------------------------------------------------------

# Db EIBSS table
date_pairs <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_EIBSS_DATE_PAIRS"))

# Get Distinct PDS info
pds_distinct <- pds_db %>%
  inner_join(
    y = date_pairs %>% select(DOB_TWO) %>% distinct()
  ) %>%
  select(FORENAME_TWO) %>%
  distinct()

# Create name pairs
name_pairs <- pds_distinct %>%
  full_join(
    res %>%
      select(FORENAME_ONE) %>%
      distinct(),
    by = character()
  ) %>%
  mutate(JW_FORENAME = UTL_MATCH.JARO_WINKLER(FORENAME_ONE, FORENAME_TWO)) %>%
  filter(JW_FORENAME >= 0.75)

# Compute date pairs: 15 seconds
name_pairs %>%
  compute(
    name = "INT600_EIBSS_NAMES_PAIRS",
    temporary = FALSE
  )

# Part 4.3 ---------------------------------------------------------------------

# Name Pairs
name_pairs <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_EIBSS_NAMES_PAIRS"))

# Create name-date pairs
name_date_pairs <- pds_db %>%
  inner_join(
    name_pairs %>%
      select(FORENAME_TWO) %>%
      distinct()
  ) %>%
  inner_join(
    date_pairs %>%
      select(DOB_TWO) %>%
      distinct()
  ) %>%
  select(ID_TWO, FORENAME_TWO, DOB_TWO) %>%
  full_join(
    res %>%
      select(ID_ONE, FORENAME_ONE, DOB_ONE),
    by = character()
  ) %>%
  inner_join(date_pairs) %>%
  inner_join(name_pairs)

# Compute date pairs
name_date_pairs %>%
  compute(
    name = "INT600_EIBSS_NAME_DATE_PAIRS",
    temporary = FALSE
  )

# Part 4.4 ---------------------------------------------------------------------

# Name Pairs: 5.8M
name_date_pairs <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_EIBSS_NAME_DATE_PAIRS"))

# Missing Confident matches
extra_matches <- name_date_pairs %>%
  inner_join(
    y = pds_db %>% select(ID_TWO, SURNAME_TWO, POSTCODE_TWO)
    ) %>%
  inner_join(
    y = res %>% select(ID_ONE, SURNAME_ONE, POSTCODE_ONE)
    ) %>%
  dplyr::mutate(
    # JW match, bypassing exact string matches (DIFF_DOB already calculated)
    JW_SURNAME = ifelse(SURNAME_ONE == SURNAME_TWO, 1, UTL_MATCH.JARO_WINKLER(SURNAME_ONE, SURNAME_TWO)),
    JW_POSTCODE = ifelse(POSTCODE_ONE == POSTCODE_TWO, 1, UTL_MATCH.JARO_WINKLER(POSTCODE_ONE, POSTCODE_TWO)),
    # Generate confident matches
    MATCH_TYPE = dplyr::case_when(
      (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DIFF_DOB == 0) ~ "Exact",
      (JW_SURNAME == 1 & JW_FORENAME == 1 & DIFF_DOB == 0) ~ "Confident",
      (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DIFF_DOB <= 2) ~ "Confident",
      (JW_FORENAME == 1 & JW_POSTCODE == 1 & DIFF_DOB == 0) ~ "Confident",
      (JW_SURNAME == 1 & JW_FORENAME >= 0.75 & JW_POSTCODE == 1 & DIFF_DOB == 0) ~ "Confident",
      (JW_SURNAME >= 0.85 & JW_FORENAME >= 0.75 & JW_POSTCODE >= 0.85 & DIFF_DOB <= 2) ~ "Confident",
      TRUE ~ "No Match"
    )
  )

# Compute date pairs
extra_matches %>%
  compute(
    name = "INT600_EIBSS_EXTRA_MATCHES",
    temporary = FALSE
  )

# Part 4.5 Analysis match results -----------------------------------------------

# Name Pairs: 5.8M
extra_matches <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_EIBSS_EXTRA_MATCHES"))

extra_matches

# Potential new Rule?
extra_matches %>%
  filter(DIFF_DOB <= 2) %>%
  filter(JW_POSTCODE >= 0.85) %>%
  filter(
    INSTR(FORENAME_ONE, FORENAME_TWO) > 0 |
      INSTR(FORENAME_TWO, FORENAME_ONE) > 0
  ) %>%
  filter(JW_SURNAME >= 0.85)

extra_matches %>%
  filter(DIFF_DOB <= 0) %>%
  filter(JW_POSTCODE == 1) %>%
  filter(
    INSTR(SURNAME_ONE, SURNAME_TWO) > 0 |
      INSTR(SURNAME_TWO, SURNAME_ONE) > 0
  ) %>%
  filter(JW_FORENAME >= 0.75)

#---------------------------------------------

extra_matches %>%
  filter(DIFF_DOB <= 2) %>%
  filter(JW_POSTCODE >= 0.85) %>%
  filter(
    INSTR(FORENAME_ONE, FORENAME_TWO) > 0 |
      INSTR(FORENAME_TWO, FORENAME_ONE) > 0
  ) %>%
  filter(JW_SURNAME >= 0.85)

extra_matches %>%
  filter(DIFF_DOB <= 0) %>%
  filter(JW_POSTCODE == 1) %>%
  filter(
    INSTR(FORENAME_ONE, FORENAME_TWO) > 0 |
      INSTR(FORENAME_TWO, FORENAME_ONE) > 0
  ) %>%
  filter(JW_SURNAME >= 0.85)

# Disconnect
DBI::dbDisconnect()


