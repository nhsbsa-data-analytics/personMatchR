
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
leap_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("DALL_REF", "INT600_DWP_LEAP_FORMAT"))

# Db eibss table
pds_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("STBUC", "INT617_TMP_PDS"))

# Tally
leap_db %>% tally()
pds_db %>% tally()

# Format LEAP data
leap <- leap_db %>%
  format_db_postcode(., POSTCODE) %>%
  format_db_name(., FORENAME) %>%
  format_db_name(., SURNAME) %>%
  format_db_date(., DATE_OF_BIRTH)

# Format PDS data
pds <- pds_db %>%
  select(RECORD_ID, DOB, SURNAME, FORENAME, POSTCODE, NHS_NO_PDS, DOD) %>%
  format_db_postcode(., POSTCODE) %>%
  format_db_name(., FORENAME) %>%
  format_db_name(., SURNAME) %>%
  format_db_date(., DOB)

# Write the table back to the DB: 1 min
leap %>%
  compute(
    name = "INT600_LEAP_PROCESSED",
    temporary = FALSE
  )

# Write the table back to the DB: 35 mins
pds %>%
  compute(
    name = "INT600_PDS_PROCESSED",
    temporary = FALSE
  )

# Disconnect
DBI::dbDisconnect(con)


#-------------------------------------------------------------------------------
# Part Two: Function output

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db pds table
leap_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_LEAP_PROCESSED"))

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

# Check data
leap_db
eib_db
pds_db

# Function output
results <- find_db_matches(
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
  format_data = FALSE
)

# Write data back
Sys.time()
results %>%
  compute(
    name = "INT600_EIBSS_TEST2",
    temporary = FALSE
  )
Sys.time()



#-------------------------------------------------------------------------------
# Part Three: Manual Code Run

# Check data
df_one
df_two

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
  calc_permutations(., FORENAME_ONE, SURNAME_ONE, POSTCODE_ONE, DOB_ONE)

# Select columns and calculate permutations
df_two <- df_two %>%
  calc_permutations(., FORENAME_TWO, SURNAME_TWO, POSTCODE_TWO, DOB_TWO)

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

# Get results: 15 mins
match_df <- all_matches %>% collect()

#
match_df %>%
  filter(MATCH_TYPE == "No Match") %>%
  select(ID_ONE) %>%
  distinct()
  count(MATCH_TYPE)
