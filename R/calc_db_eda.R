
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
  calc_permutations(., FORENAME, SURNAME, POSTCODE, DOB) %>%
  calc_alpha_permutations(., FORENAME, SURNAME, POSTCODE)

# Format PDS data
pds <-pds_db %>%
  select(RECORD_ID, DOB, SURNAME, FORENAME, POSTCODE) %>%
  format_db_postcode_simple(., POSTCODE) %>%
  format_db_name(., FORENAME) %>%
  format_db_name(., SURNAME) %>%
  format_db_date(., DOB) %>%
  calc_permutations(., FORENAME, SURNAME, POSTCODE, DOB) %>%
  calc_alpha_permutations(., FORENAME, SURNAME, POSTCODE)

# Write the table back to the DB:
eib %>%
  compute(
    name = "INT617_EIB_PROCESSED",
    temporary = FALSE
  )
Sys.time()
# Write the table back to the DB: 45 mins
pds %>%
  compute(
    name = "INT617_PDS_PROCESSED",
    temporary = FALSE
  )

# Disconnect
DBI::dbDisconnect(con)

Sys.time()

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
  select(REFERENCE, DOB, SURNAME, FORENAME, POSTCODE) %>%
  inner_join(
    y = pds_db %>% select(RECORD_ID, DOB, SURNAME, FORENAME, POSTCODE)
  ) %>%
  select(REFERENCE, RECORD_ID) %>%
  distinct()

# Remaining records
eib <- eib_db %>%
  anti_join(y = exact, by = "REFERENCE")

# Permutation join
perm_join <- function(df1, df2, id_one, id_two, perm_num){

  output <- inner_join(
    x = df1 %>% select({{ id_one}}, {{ perm_num }}),
    y = df2 %>% select({{ id_two}}, {{ perm_num }})
  ) %>%
    select(-{{ perm_num }})
  return(output)
}

# Calc permutations
eib <- eib %>%
  calc_permutations(., FORENAME, SURNAME, POSTCODE, DOB)

pds_db <- pds_db %>%
  calc_permutations(., FORENAME, SURNAME, POSTCODE, DOB)

# Get list of lookup pairs
id_pairs <- perm_join(eib, pds_db, REFERENCE, RECORD_ID, PERM1) %>%
  union_all(
    perm_join(eib, pds_db, REFERENCE, RECORD_ID, PERM2)
  ) %>%
  union_all(
    perm_join(eib, pds_db, REFERENCE, RECORD_ID, PERM3)
  ) %>%
  union_all(
    perm_join(eib, pds_db, REFERENCE, RECORD_ID, PERM4)
    ) %>%
  union_all(
    perm_join(eib, pds_db, REFERENCE, RECORD_ID, PERM5)
  ) %>%
  union_all(
    perm_join(eib, pds_db, REFERENCE, RECORD_ID, PERM6)
  ) %>%
  union_all(
    perm_join(eib, pds_db, REFERENCE, RECORD_ID, PERM7)
  ) %>%
  union_all(
    perm_join(eib, pds_db, REFERENCE, RECORD_ID, PERM8)
  ) %>%
  union_all(
    perm_join(eib, pds_db, REFERENCE, RECORD_ID, PERM9)
  ) %>%
  select(REFERENCE, RECORD_ID) %>%
  distinct() %>%
  tally()






output <- id_pairs %>%
  inner_join(
    y = eib %>% select(REFERENCE, DOB, SURNAME, FORENAME, POSTCODE),
    by = "REFERENCE"
  ) %>%
  inner_join(
    y = pds_db %>% select(RECORD_ID, DOB, SURNAME, FORENAME, POSTCODE),
    by = "RECORD_ID"
  ) %>%
  collect()


output

output %>%
  mutate(FLAG1 = ifelse(substr(POSTCODE.x,1,3) != substr(POSTCODE.y,1,3), 1, 0)) %>%
  mutate(FLAG2 = ifelse(substr(SURNAME.x,1,3) != substr(SURNAME.y,1,3), 1, 0)) %>%
  filter(
    FLAG1 == 0,
    FLAG2 == 0
    ) %>%
  tally()

# Disconnect
DBI::dbDisconnect(con)

Sys.time()



#-------------------------------------------------------------------------------
# Part Three: Generate list of relevant dates

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db pds table
pds_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("STBUC", "INT617_TMP_PDS"))

# Db eibss table
eib_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("STBUC", "INT617_TMP_EIBSS"))

# Exact matches
exact <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT617_EXACT"))

# Format and select dob, tmp for full-join
eib <- eib_db %>%
  anti_join(y = exact, by = "REFERENCE") %>%
  format_db_date(., DOB) %>%
  select(DOB_ONE = DOB) %>%
  distinct() %>%
  mutate(TMP = 1)

# Format and select dob, tmp for full-join
pds <- pds_db %>%
  format_db_date(., DOB) %>%
  select(DOB_TWO = DOB) %>%
  distinct() %>%
  mutate(TMP = 1)

# Generate lost of dates
dob <- eib %>%
  full_join(pds) %>%
  dob_lv_filter(., DOB_ONE, DOB_TWO) %>%
  select(-TMP)


# Write back to db
dob %>%
  compute(
    name = "INT617_DOB_DIST",
    temporary = FALSE
  )

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
# Part Six: Implement manual JW

data <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT623_PDS_JW_FORENAMES"))

data <- data %>%
  mutate(ID = row_number(FORENAME_ONE)) %>%
  filter(FORENAME_ONE == "NATALIJA") %>%
  filter(FORENAME_TWO == "AURELIJA")

a <- data %>%
  mutate(TOKEN_TWO = trimws(REGEXP_REPLACE(FORENAME_TWO, '*', ' '))) %>%
  oracle_unnest_tokens(col = 'TOKEN_TWO') %>%
  select(ID, FORENAME_TWO, TOKEN_TWO = TOKEN_NUMBER, TOKEN)

b <- data %>%
  mutate(TOKEN_TWO = trimws(REGEXP_REPLACE(FORENAME_TWO, '*', ' '))) %>%
  oracle_unnest_tokens(col = 'TOKEN_TWO') %>%
  select(ID, FORENAME_TWO, TOKEN_TWO = TOKEN_NUMBER, TOKEN)

c <- a %>%
  inner_join(b, by = c("ID", "TOKEN")) %>%
  group_by(ID) %>%
  mutate(
    MAX = ifelse(
      test = nchar(FORENAME_ONE) >= nchar(FORENAME_TWO),
      yes = nchar(FORENAME_ONE),
      no = nchar(FORENAME_TWO)
    ),
    LEN_DIST = floor((MAX / 2) - 1),
    CHAR_DIST = ifelse(abs(TOKEN_ONE - TOKEN_TWO) <= LEN_DIST, 1, 0)
  ) %>%
  ungroup() %>%
  filter(CHAR_DIST == 1) %>%
  select(-c(MAX, LEN_DIST, CHAR_DIST))


d <- c %>%
  select(ID, FORENAME_ONE, TOKEN_ONE, TOKEN) %>%
  distinct() %>%
  group_by(ID, FORENAME_ONE, TOKEN) %>%
  mutate(
    TOKEN_REV = paste0(TOKEN, rank(desc(TOKEN_ONE))),
    TOKEN = paste0(TOKEN, rank(TOKEN_ONE))
  ) %>%
  ungroup()

e <- c %>%
  select(ID, FORENAME_TWO, TOKEN_TWO, TOKEN) %>%
  distinct() %>%
  group_by(ID, TOKEN) %>%
  mutate(
    TOKEN_REV = paste0(TOKEN, rank(desc(TOKEN_NUMBER_TWO))),
    TOKEN = paste0(TOKEN, rank(TOKEN_NUMBER_TWO))
  ) %>%
  ungroup()

t_one <- d %>%
  inner_join(e, by = c("ID", "TOKEN")) %>%
  group_by(ID) %>%
  mutate(
    M = n(),
    ONE_RANK = rank(TOKEN_NUMBER_ONE),
    TWO_RANK = rank(TOKEN_NUMBER_TWO),
    T_ONE = sum(ifelse(ONE_RANK != TWO_RANK, 1, 0)) / 2
  ) %>%
  ungroup() %>%
  select(ID, FORENAME_ONE, FORENAME_TWO, S_ONE, S_TWO, M, T_ONE) %>%
  distinct()

t_two <- d %>%
  inner_join(e, by = c("ID", "TOKEN_REV")) %>%
  group_by(ID) %>%
  mutate(
    M = n(),
    ONE_RANK = rank(TOKEN_NUMBER_ONE),
    TWO_RANK = rank(TOKEN_NUMBER_TWO),
    T_TWO = sum(ifelse(ONE_RANK != TWO_RANK, 1, 0)) / 2
  ) %>%
  ungroup() %>%
  select(ID, FORENAME_ONE, FORENAME_TWO, S_ONE, S_TWO, M, T_TWO) %>%
  distinct()

f <- t_one %>%
  inner_join(t_two) %>%
  mutate(
    T_VAL = ifelse(T_ONE <= T_TWO, T_ONE, T_TWO),
    SIM = (1/3) * (((M / S_ONE) + (M / S_TWO) + ((M - T_VAL) / M))),
    L = case_when(
      substr(FORENAME_ONE, 1, 1) != substr(FORENAME_TWO, 1, 1) ~ 0,
      substr(FORENAME_ONE, 1, 4) == substr(FORENAME_TWO, 1, 4) ~ 4,
      substr(FORENAME_ONE, 1, 3) == substr(FORENAME_TWO, 1, 3) ~ 3,
      substr(FORENAME_ONE, 1, 2) == substr(FORENAME_TWO, 1, 2) ~ 2,
      substr(FORENAME_ONE, 1, 1) == substr(FORENAME_TWO, 1, 1) ~ 1
    ),
    JW = SIM + (L * 0.1 * (1 - SIM))
  ) %>%
  ungroup() %>%
  collect()

Sys.time()

#-------------------------------------------------------------------------------
# Part Seven: The 'normal' workflow

exact <- pds1 %>%
  inner_join(
    y = pds2,
    by = c(
      "SURNAME" = "SURNAME",
      "FORENAME" = "FORENAME",
      "DOB" = "DOB",
      "POSTCODE" = "POSTCODE"
    ),
    na_matches = "never"
  )

exact_rev <- pds1 %>%
  # join where all fields match exactly
  dplyr::inner_join(
    y = pds2,
    by = c(
      "FORENAME" = "SURNAME",
      "SURNAME" = "FORENAME",
      "DOB" = "DOB",
      "POSTCODE" = "POSTCODE"
    ),
    na_matches = "never"
  )

# Generate features for all exact matches
exact <- exact %>%
  dplyr::union_all(exact_rev) %>%
  # by default all matches are an "exact" match with perfect scores
  dplyr::mutate(
    JW_SURNAME = 1,
    JW_FORENAME = 1,
    JW_POSTCODE = 1,
    ED_DOB = 0,
    MATCH_TYPE = 'Exact'
  ) %>%
  # select only the key fields
  dplyr::select(
    ID.x, ID.y, JW_SURNAME, JW_FORENAME, JW_POSTCODE, ED_DOB, MATCH_TYPE
  )

pds1 <- pds1 %>%
  anti_join(exact, by = c("ID" = "ID.x"))

cross <- full_join(
  x = pds1 %>% mutate(TMP = 1),
  y = pds2 %>% mutate(TMP = 1),
  by = "TMP"
  ) %>%
  select(-TMP) %>%
  dplyr::filter(
    # Tokens share the same first letter
    SUBSTR(FORENAME.x, 1, 1) == SUBSTR(FORENAME.y, 1, 1) |
      # Tokens share same second letter
      SUBSTR(FORENAME.x, 2, 1) == SUBSTR(FORENAME.y, 2, 1) |
      # Tokens share same last letter
      SUBSTR(FORENAME.x, LENGTH(FORENAME.x), 1) == SUBSTR(FORENAME.y, LENGTH(FORENAME.y), 1) |
      # One token is a substring of the other
      INSTR(FORENAME.x, FORENAME.y) > 1 |
      INSTR(FORENAME.y, FORENAME.x) > 1
  ) %>%
  dob_lv_filter()

jw_join <- cross %>%
  select(FORENAME.x, FORENAME.y) %>%
  distinct() %>%
  mutate(JW = UTL_MATCH.JARO_WINKLER(FORENAME.x, FORENAME.y)) %>%
  filter(JW >= 0.75) %>%
  select(-JW)

cross_join <- cross %>%
  inner_join(jw_join)
#inner_join(lv_join)

match <- cross_join %>%
  mutate(
    #JW_FORENAME = UTL_MATCH.JARO_WINKLER(FORENAME.x, FORENAME.y),
    JW_FORENAME = 0.75,
    JW_SURNAME = UTL_MATCH.JARO_WINKLER(SURNAME.x, SURNAME.y),
    JW_POSTCODE = UTL_MATCH.JARO_WINKLER(POSTCODE.x, POSTCODE.y),
    ED_DOB = 0
    #ED_DOB = UTL_MATCH.JARO_WINKLER(DOB.x, DOB.y)
  ) %>%
  # limit to key fields and score matches
  dplyr::select(ID.x, ID.y, JW_SURNAME, JW_FORENAME, JW_POSTCODE, ED_DOB) %>%
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

all_match <- match %>%
  dplyr::union_all(exact) %>%
  dplyr::group_by(ID.x) %>%
  dplyr::mutate(MATCH_COUNT = n()) %>%
  dplyr::ungroup() %>%
  dplyr::select(ID.x, MATCH_COUNT, ID.y, MATCH_TYPE)

Sys.time()
# Write the table back to the DB with indexes
all_match %>%
  compute(
    name = "INT623_PDS_MATCH",
    temporary = FALSE
  )
Sys.time()
# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
