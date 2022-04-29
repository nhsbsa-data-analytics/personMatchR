
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
# Part Two: composite joins then scoring

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db pds table
leap_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_LEAP_PROCESSED"))

# Db eibss table
pds_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_PDS_PROCESSED"))

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

# Results
results <- find_db_matches(
  leap_db, ID, FORENAME, SURNAME, DATE_OF_BIRTH, POSTCODE,
  pds_db, RECORD_ID, FORENAME_PDS, SURNAME_PDS, DOB_PDS, POSTCODE_PDS,
  "match", F
  )

Sys.time()
# Write the table back to the DB with indexes: ~ 8hrs
results %>%
  compute(
    name = "INT600_LEAP_TEST3",
    temporary = FALSE
  )
Sys.time()
# Results: ~ 10 mins



results <- find_db_matches(
  leap_db, ID, FORENAME, SURNAME, DATE_OF_BIRTH, POSTCODE,
  pds_db, RECORD_ID, FORENAME_PDS, SURNAME_PDS, DOB_PDS, POSTCODE_PDS,
  "match", F
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
# Part Three: manual woring of code

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db pds table
df_one <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_LEAP_PROCESSED"))

# Db eibss table
df_two <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT600_PDS_PROCESSED"))

# For convenience rename columns
df_one <- df_one %>%
  rename(
    ID_ONE = ID,
    FORENAME_ONE = FORENAME,
    SURNAME_ONE = SURNAME,
    DOB_ONE = DATE_OF_BIRTH,
    POSTCODE_ONE = POSTCODE
  )

# For convenience rename columns
df_two <- df_two %>%
  rename(
    ID_TWO = RECORD_ID,
    FORENAME_TWO = FORENAME,
    SURNAME_TWO = SURNAME,
    DOB_TWO = DOB,
    POSTCODE_TWO = POSTCODE
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
      dplyr::select(df_one_cols, {{.x}}) %>%
      dplyr::inner_join(
        df_two %>%
          dplyr::select(df_two_cols, {{.x}}),
        by = {{.x}}
      ) %>%
      dplyr::select(- {{.x}})
  }) %>%
  purrr::reduce(function(x, y) union(x, y)) %>%
  dplyr::distinct()

# Generate list of feasible dob-pairs with 6 identical characters
cross <- id_pairs %>%
  name_db_filter(., FORENAME_ONE, FORENAME_TWO) %>%
  dob_lv_filter(., DOB_ONE, DOB_TWO)

# Generate a list
matches <- cross %>%
  dplyr::mutate(
    # NAs for zeros
    JW_FORENAME = ifelse(FORENAME_ONE == FORENAME_TWO, 1, UTL_MATCH.JARO_WINKLER(FORENAME_ONE, FORENAME_TWO)),
    JW_SURNAME = ifelse(SURNAME_ONE == SURNAME_TWO, 1, UTL_MATCH.JARO_WINKLER(SURNAME_ONE, SURNAME_TWO)),
    JW_POSTCODE = ifelse(POSTCODE_ONE == POSTCODE_TWO, 1, UTL_MATCH.JARO_WINKLER(POSTCODE_ONE, POSTCODE_TWO)),
    ED_DOB = ifelse(DOB_ONE == DOB_TWO, 1, UTL_MATCH.EDIT_DISTANCE(DOB_ONE, DOB_TWO)),
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
  dplyr::union_all(exact_matches)

# Determine non-matches
non_matches <- df_one %>%
  dplyr::anti_join(y = matches %>% dplyr::select(ID_ONE))

# Get row counts
non_matches_tally <- non_matches %>% tally()
df_one_tally <- df_one %>% tally()

# Final cross-join
cross_join_size <- non_match_tally * df_one_tally

#Less than 1 billion then attempt cross join
if(cross_join_size <= 1000000000){

    # Final cross-join matches, with corss-join threshold in place
    final_matches <- non_matches %>%
      dplyr::mutate(TMP = 1) %>%
      dplyr::full_join(
        y = df_two %>%
          dplyr::select(df_two_cols) %>%
          dplyr::mutate(TMP = 1),
        by = "TMP"
      ) %>%
      select(-TMP) %>%
      dplyr::mutate(
        # NAs for zeros
        JW_FORENAME = ifelse(FORENAME_ONE == FORENAME_TWO, 1, UTL_MATCH.JARO_WINKLER(FORENAME_ONE, FORENAME_TWO)),
        JW_SURNAME = ifelse(SURNAME_ONE == SURNAME_TWO, 1, UTL_MATCH.JARO_WINKLER(SURNAME_ONE, SURNAME_TWO)),
        JW_POSTCODE = ifelse(POSTCODE_ONE == POSTCODE_TWO, 1, UTL_MATCH.JARO_WINKLER(POSTCODE_ONE, POSTCODE_TWO)),
        ED_DOB = ifelse(DOB_ONE == DOB_TWO, 1, UTL_MATCH.EDIT_DISTANCE(DOB_ONE, DOB_TWO)),
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
      dplyr::filter(MATCH_TYPE != "No Match")

    # Re-determine non-matches
    non_matches <- non_matches %>%
      dplyr::anti_join(y = final_matches %>% dplyr::select(ID_ONE))

    # Re-determine total matches df
    matches <- matches %>%
      # Add exact matches
      dplyr::union_all(final_matches)

}
  # Determine non-matches
  non_matches <- non_matches %>%
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
  dplyr::ungroup() %>%
  dplyr::select(
    ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE,
    ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
    MATCH_TYPE, MATCH_COUNT
  ) %>%
  #Rejoin original columns
  dplyr::left_join(df_one_cols, by = "ID_ONE") %>%
  dplyr::left_join(df_two_cols, by = "ID_TWO")








  # Rename back to OG column names
  dplyr::rename(
    {{ id_one }} := ID_ONE,
    {{ forename_one }} := FORENAME_ONE,
    {{ surname_one }} := SURNAME_ONE,
    {{ dob_one }} := DOB_ONE,
    {{ postcode_one }} := POSTCODE_ONE,
    {{ id_two }} := ID_TWO,
    {{ forename_two }} := FORENAME_TWO,
    {{ surname_two }} := SURNAME_ONE,
    {{ dob_two }} := DOB_TWO,
    {{ postcode_two }} := POSTCODE_TWO
  )

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

# Get row counts
non_matches_tally <- leap_db %>% tally() %>% pull()
df_one_tally <- pds_db %>% tally() %>% pull()

# Final cross-join
cross_join_size <- non_matches_tally * df_one_tally

#Less than 1 billion then attempt cross join
if(cross_join_size <= 1000000000){
  print("hello")
}else{
  print("no")
}
