
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

# Rename PDS relevant variables
pds_db <- pds_db %>%
  rename(
    DOB_PDS = DOB,
    SURNAME_PDS = SURNAME,
    FORENAME_PDS = FORENAME,
    POSTCODE_PDS = POSTCODE
  )

# Function to find all matches form the db
find_db_matches <- function(
  df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
  df_two, id_two, forename_two, surname_two, dob_two, postcode_two
  ){

  # For convenience rename columns
  df_one <- df_one %>%
    rename(
      ID_ONE := {{ id_one }},
      FORENAME_ONE := {{ forename_one }},
      SURNAME_ONE := {{ surname_one }},
      DOB_ONE := {{ dob_one }},
      POSTCODE_ONE := {{ postcode_one }}
      )

  # For convenience rename columns
  df_two <- df_two %>%
    rename(
      ID_TWO := {{ id_two }},
      FORENAME_TWO := {{ forename_two }},
      SURNAME_TWO := {{ surname_two }},
      DOB_TWO := {{ dob_two }},
      POSTCODE_TWO := {{ postcode_two }}
    )

  # Exact matches
  exact <- df_one %>%
    select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE) %>%
    inner_join(
      y = df_two %>%
        select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO),
      by = c(
        "FORENAME_ONE" = "FORENAME_TWO",
        "SURNAME_ONE" = "SURNAME_TWO",
        "DOB_ONE" = "DOB_TWO",
        "POSTCODE_ONE" = "POSTCODE_TWO"
        )
    ) %>%
    distinct() %>%
    mutate(
      FORENAME_TWO = FORENAME_ONE,
      SURNAME_TWO = SURNAME_ONE,
      DOB_TWO = DOB_ONE,
      POSTCODE_TWO = POSTCODE_ONE,
      JW_FORENAME = 1,
      JW_SURNAME = 1,
      JW_POSTCODE = 1,
      ED_DOB = 0,
      MATCH_TYPE = 'Exact',
    ) %>%
    select(ID_ONE, ID_TWO, everything())

  # Remaining records
  df_one <- df_one %>%
    anti_join(y = exact, by = "ID_ONE")

  # Permutation join
  perm_join <- function(df_one, df_two, id_one, id_two, perm_num){

    output <- inner_join(
      x = df_one %>% select({{ id_one}}, {{ perm_num }}),
      y = df_two %>% select({{ id_two}}, {{ perm_num }})
    ) %>%
      select(-{{ perm_num }})
    return(output)
  }

  # Get list of lookup pairs
  id_pairs <- perm_join(
    df_one = df_one,
    df_two = df_two,
    id_one = ID_ONE,
    id_two = ID_TWO,
    perm_num = PERM1
    ) %>%
    union_all(
      perm_join(
        df_one = df_one,
        df_two = df_two,
        id_one = ID_ONE,
        id_two = ID_TWO,
        perm_num = PERM2
      )
    ) %>%
    union_all(
      perm_join(
        df_one = df_one,
        df_two = df_two,
        id_one = ID_ONE,
        id_two = ID_TWO,
        perm_num = PERM3
      )
    ) %>%
    union_all(
      perm_join(
        df_one = df_one,
        df_two = df_two,
        id_one = ID_ONE,
        id_two = ID_TWO,
        perm_num = PERM4
      )
    ) %>%
    union_all(
      perm_join(
        df_one = df_one,
        df_two = df_two,
        id_one = ID_ONE,
        id_two = ID_TWO,
        perm_num = PERM5
      )
    ) %>%
    union_all(
      perm_join(
        df_one = df_one,
        df_two = df_two,
        id_one = ID_ONE,
        id_two = ID_TWO,
        perm_num = PERM6
      )
    ) %>%
    union_all(
      perm_join(
        df_one = df_one,
        df_two = df_two,
        id_one = ID_ONE,
        id_two = ID_TWO,
        perm_num = PERM7
      )
    ) %>%
    union_all(
      perm_join(
        df_one = df_one,
        df_two = df_two,
        id_one = ID_ONE,
        id_two = ID_TWO,
        perm_num = PERM8
      )
    ) %>%
    union_all(
      perm_join(
        df_one = df_one,
        df_two = df_two,
        id_one = ID_ONE,
        id_two = ID_TWO,
        perm_num = PERM9
      )
    ) %>%
    select(ID_ONE, ID_TWO) %>%
    distinct()

  # Remove Permutation info from matching dfs
  df_one <- df_one %>%
    select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE)

  # Remove Permutation info from matching dfs
  df_two <- df_two %>%
    select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO)

  # Join lookup back to original dfs
  id_pairs <- id_pairs %>%
    inner_join(y = df_one, by = "ID_ONE") %>%
    inner_join(y = df_two, by = "ID_TWO")

  # Generate list of feasible dob-pairs with 6 identical characters
  eligible_dates <- dob_lv_filter(
    df = id_pairs,
    dob_one = DOB_ONE,
    dob_two = DOB_TWO
    )

  # Generate a list of feasible forename pairs
  eligible_names <- calc_db_jw_threshold(
    df = id_pairs,
    name_one = FORENAME_ONE,
    name_two = FORENAME_TWO,
    threshold_val = 0.75,
    col_name = 'JW_FORENAME'
    )

  # Filter permutation output by feasible date-pair list
  cross <- id_pairs %>%
    inner_join(eligible_dates) %>%
    inner_join(eligible_names)

  # Generate a list
  match <- cross %>%
    mutate(
      JW_SURNAME = UTL_MATCH.JARO_WINKLER(SURNAME_ONE, SURNAME_TWO),
      JW_POSTCODE = UTL_MATCH.JARO_WINKLER(POSTCODE_ONE, POSTCODE_TWO),
      ED_DOB = ifelse(DOB_ONE == DOB_TWO, 0, 2)
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

  # Final Results
  all_match <- match %>%
    dplyr::union_all(exact) %>%
    dplyr::group_by(ID_ONE) %>%
    dplyr::mutate(MATCH_COUNT = dplyr::n_distinct(ID_TWO)) %>%
    dplyr::ungroup() %>%
    # Rename back to OG column names
    rename(
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
}

find_db_matches(
  eib_db, REFERENCE, FORENAME, SURNAME, DOB, POSTCODE,
  pds_db, RECORD_ID, FORENAME_PDS, SURNAME_PDS, DOB_PDS, POSTCODE_PDS
)


# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
