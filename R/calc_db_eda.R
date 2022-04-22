
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

# Format EIBBS data
leap <- leap_db %>%
  select(ID, DATE_OF_BIRTH, SURNAME, FORENAME, POSTCODE) %>%
  format_db_postcode(., POSTCODE) %>%
  format_db_name(., FORENAME) %>%
  format_db_name(., SURNAME) %>%
  format_db_date(., DATE_OF_BIRTH) %>%
  calc_permutations(., FORENAME, SURNAME, POSTCODE, DATE_OF_BIRTH)

# Format PDS data
pds <- pds_db %>%
  select(RECORD_ID, DOB, SURNAME, FORENAME, POSTCODE) %>%
  format_db_postcode(., POSTCODE) %>%
  format_db_name(., FORENAME) %>%
  format_db_name(., SURNAME) %>%
  format_db_date(., DOB) %>%
  calc_permutations(., FORENAME, SURNAME, POSTCODE, DOB)

# Write the table back to the DB: 1 min
leap %>%
  compute(
    name = "INT623_LEAP_PROCESSED",
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
leap_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT623_LEAP_PROCESSED"))

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

# Check data
pds_db
leap_db

# Results: ~ 10 mins
results <- find_db_matches(
  leap_db, ID, FORENAME, SURNAME, DATE_OF_BIRTH, POSTCODE,
  pds_db, RECORD_ID, FORENAME_PDS, SURNAME_PDS, DOB_PDS, POSTCODE_PDS,
  "all"
  )

Sys.time()
# Write the table back to the DB with indexes: ~ 8hrs
results %>%
  compute(
    name = "INT623_LEAP_TEST1",
    temporary = FALSE
  )

Sys.time()

# Results: ~ 10 mins
results <- find_db_matches(
  leap_db, ID, FORENAME, SURNAME, DATE_OF_BIRTH, POSTCODE,
  pds_db, RECORD_ID, FORENAME_PDS, SURNAME_PDS, DOB_PDS, POSTCODE_PDS,
  "match"
)

Sys.time()
# Write the table back to the DB with indexes: ~ 8hrs
results %>%
  compute(
    name = "INT623_LEAP_TEST2",
    temporary = FALSE
  )

Sys.time()

# Results: ~ 10 mins
results <- find_db_matches(
  leap_db, ID, FORENAME, SURNAME, DATE_OF_BIRTH, POSTCODE,
  pds_db, RECORD_ID, FORENAME_PDS, SURNAME_PDS, DOB_PDS, POSTCODE_PDS,
  "key"
)

Sys.time()
# Write the table back to the DB with indexes: ~ 8hrs
results %>%
  compute(
    name = "INT623_LEAP_TEST3",
    temporary = FALSE
  )

Sys.time()

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db pds table
leap_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT623_LEAP_PROCESSED"))

# Db eibss table
pds_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT617_PDS_PROCESSED"))

# For convenience rename columns
leap_db <- leap_db %>%
  rename(
    ID_ONE = ID,
    FORENAME_ONE = FORENAME,
    SURNAME_ONE = SURNAME,
    DOB_ONE = DATE_OF_BIRTH,
    POSTCODE_ONE = POSTCODE
  )

# For convenience rename columns
pds_db <- pds_db %>%
  rename(
    ID_TWO = RECORD_ID,
    FORENAME_TWO = FORENAME,
    SURNAME_TWO = SURNAME,
    DOB_TWO = DOB,
    POSTCODE_TWO = POSTCODE
  )
pds_db %>% tally()
# Exact matches
exact_matches <- leap_db %>%
  dplyr::select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE) %>%
  dplyr::inner_join(
    y = pds_db %>%
      dplyr::select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO),
    by = c(
      "FORENAME_ONE" = "FORENAME_TWO",
      "SURNAME_ONE" = "SURNAME_TWO",
      "DOB_ONE" = "DOB_TWO",
      "POSTCODE_ONE" = "POSTCODE_TWO"
    )
  )

# Exact matches
exact_matches_reverse <- leap_db %>%
  dplyr::select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE) %>%
  dplyr::inner_join(
    y = pds_db %>%
      dplyr::select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO),
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
remain <- leap_db %>%
  dplyr::anti_join(y = exact_matches, by = "ID_ONE")

# List of permutation-join columns
perm_num <- paste0("PERM", 1:9)

# Distinct list of ID perm-join pairs
id_pairs <- perm_num %>%
  purrr::map(~{

    remain %>%
      dplyr::select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE, {{.x}}) %>%
      dplyr::inner_join(
        pds_db %>%
          dplyr::select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO, {{.x}})
      ) %>%
      dplyr::select(- {{.x}})
  }) %>%
  purrr::reduce(function(x, y) union(x, y)) %>%
  dplyr::distinct()
id_pairs

# Generate list of feasible dob-pairs with 6 identical characters
cross <- id_pairs %>%
  name_db_filter(., FORENAME_ONE, FORENAME_TWO) %>%
  dob_lv_filter(., DOB_ONE, DOB_TWO)

# Generate a list
matches <- cross %>%
  dplyr::mutate(
    # NAs for zeros
    JW_FORENAME = UTL_MATCH.JARO_WINKLER(FORENAME_ONE, FORENAME_TWO),
    JW_SURNAME = UTL_MATCH.JARO_WINKLER(SURNAME_ONE, SURNAME_TWO),
    JW_POSTCODE = UTL_MATCH.JARO_WINKLER(POSTCODE_ONE, POSTCODE_TWO),
    ED_DOB = ifelse(DOB_ONE == DOB_TWO, 0, 2),
    # Generate confident matches
    MATCH_TYPE = dplyr::case_when(
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
  dplyr::filter(MATCH_TYPE != "No Match") %>%
  # Add exact matches
  dplyr::union_all(exact)

# Determine non-matches
non_matches <- leap_db %>%
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
  # Calculate match_count per primary dfID
  dplyr::group_by(ID_ONE) %>%
  dplyr::mutate(MATCH_COUNT = dplyr::n_distinct(ID_TWO)) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE,
    ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    MATCH_TYPE, MATCH_COUNT
    )
  # Rename back to OG column names
  # dplyr::rename(
  #   {{ id_one }} := ID_ONE,
  #   {{ forename_one }} := FORENAME_ONE,
  #   {{ surname_one }} := SURNAME_ONE,
  #   {{ dob_one }} := DOB_ONE,
  #   {{ postcode_one }} := POSTCODE_ONE,
  #   {{ id_two }} := ID_TWO,
  #   {{ forename_two }} := FORENAME_TWO,
  #   {{ surname_two }} := SURNAME_ONE,
  #   {{ dob_two }} := DOB_TWO,
  #   {{ postcode_two }} := POSTCODE_TWO
  # )

# Return data
return(all_match)

# Write the table back to the DB with indexes: ~ 8hrs
Sys.time()
all_matches %>%
  compute(
    name = "INT623_LEAP_TEST",
    temporary = FALSE
  )
Sys.time()
