
# Library
library(dplyr)
library(dbplyr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Functions
source("R/calc_db_functions.R")
source("R/format_db_functions.R")

#-------------------------------------------------------------------------------
# Part One: DOB Dist lookup table

pds1 <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("DIM", "DALL_PDS_PATIENT_DIM"))

pds1 <- pds1 %>%
  select(DOB_ONE = DOB) %>%
  mutate(
    DOB_ONE = as.numeric(TO_CHAR(DOB_ONE, "YYYYMMDD")),
    DOB_ONE = ifelse(nchar(DOB_ONE) == 0, NA, DOB_ONE),
    TMP = 1
  ) %>%
  distinct()

pds2 <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("DIM", "DALL_PDS_PATIENT_DIM_TO_DELETE"))

pds2 <- pds2 %>%
  select(DOB_TWO = DOB) %>%
  mutate(
    DOB_TWO = as.numeric(TO_CHAR(DOB_TWO, "YYYYMMDD")),
    DOB_TWO = ifelse(nchar(DOB_TWO) == 0, NA, DOB_TWO),
    TMP = 1
  ) %>%
  distinct()

dob <- pds1 %>%
  full_join(pds2, by = "TMP") %>%
  dob_lv_filter()

# Write the table back to the DB
dob %>%
  compute(
    name = "INT623_PDS_NAMES_DIST",
    temporary = FALSE
  )

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
# Part Two: Generate Forename Lookup using

pds1 <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("DIM", "DALL_PDS_PATIENT_DIM"))

pds2 <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("DIM", "DALL_PDS_PATIENT_DIM_TO_DELETE"))

dob <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT623_PDS_NAMES_DIST"))

pds1 <- pds1 %>%
  mutate(
    ROW = rank(NHS_NO_PDS),
    DOB = as.numeric(TO_CHAR(DOB, "YYYYMMDD")),
    DOB = ifelse(nchar(DOB) == 0, NA, DOB),
    FORENAME = toupper(REGEXP_REPLACE(FORENAME, "[^[:alpha:]]", "")),
    FORENAME = ifelse(nchar(FORENAME) == 0, NA, FORENAME),
    TMP = 1
  ) %>%
  filter(ROW <= 100000) %>%
  select(DOB_ONE = DOB, FORENAME_ONE = FORENAME, TMP) %>%
  distinct()

pds2 <- pds2 %>%
  mutate(
    ROW = rank(NHS_NO_PDS),
    DOB = as.numeric(TO_CHAR(DOB, "YYYYMMDD")),
    DOB = ifelse(nchar(DOB) == 0, NA, DOB),
    FORENAME = toupper(REGEXP_REPLACE(FORENAME, "[^[:alpha:]]", "")),
    FORENAME = ifelse(nchar(FORENAME) == 0, NA, FORENAME),
    TMP = 1
  ) %>%
  filter(ROW <= 100000) %>%
  select(DOB_TWO = DOB, FORENAME_TWO = FORENAME, TMP, POSTCODE) %>%
  distinct()

cross <- pds2 %>%
  full_join(pds1, by = "TMP") %>%
  inner_join(dob) %>%
  name_db_filter(FORENAME_ONE, FORENAME_TWO) %>%
  group_by(FORENAME_ONE, FORENAME_TWO) %>%
  summarise(TMP = sum(TMP)) %>%
  ungroup()

output <- calc_db_jw_threshold(
  df = cross,
  name_one = FORENAME_ONE,
  name_two = FORENAME_TWO,
  threshold_val = 0.75
  )

# Write the table back to the DB with indexes
output %>%
  compute(
    name = "INT623_PDS_JW_FORENAMES",
    temporary = FALSE
  )

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
# Part Three: Implement manual JW









cross_edit <- cross %>%
  mutate(ID = row_number(FORENAME_ONE)) %>%
  rename(
    NAME_ONE = FORENAME_ONE,
    NAME_TWO = FORENAME_TWO
  ) %>%
  select(ID, NAME_ONE, NAME_TWO)

a <- cross_edit %>%
  mutate(TOKEN_ONE = trimws(REGEXP_REPLACE(NAME_ONE, '*', ' '))) %>%
  nhsbsaR::oracle_unnest_tokens(col = 'TOKEN_ONE') %>%
  select(ID, NAME_ONE, NUMBER_ONE = TOKEN_NUMBER, TOKEN)

b <- cross_edit %>%
  mutate(TOKEN_TWO = trimws(REGEXP_REPLACE(NAME_TWO, '*', ' '))) %>%
  nhsbsaR::oracle_unnest_tokens(col = 'TOKEN_TWO') %>%
  select(ID, NAME_TWO, NUMBER_TWO = TOKEN_NUMBER, TOKEN)

a %>%
  inner_join(b, by = c("ID", "TOKEN")) %>%
  group_by(ID) %>%
  mutate(
    # Length of each name required for JW approx
    S_ONE = nchar(NAME_ONE),
    S_TWO = nchar(NAME_TWO),
    # Max Distance Calculation
    MAX = ifelse(S_ONE >= S_TWO, S_ONE, S_TWO),
    DIST = floor(MAX / 2) - 1,
    M = ifelse(abs(NUMBER_ONE - NUMBER_TWO) <= DIST, 1, 0)
  ) %>%
  filter(M == 1) %>%
  ungroup() %>%
  group_by(ID, TOKEN) %>%
  mutate(
    ORDER_ONE = paste0(TOKEN, dense_rank(NUMBER_ONE)),
    ORDER_TWO = paste0(TOKEN, dense_rank(NUMBER_TWO))
    ) %>%
  ungroup() %>%
  filter(ORDER_ONE == ORDER_TWO) %>%
  group_by(ID) %>%
  mutate(
    M = n(),
    # Number of transpositions (T) = 0, again to speed up approx calculation
    SIM = (1/3) * (((M / S_ONE) + (M / S_TWO) + ((M - 0) / M))),
    # Final JW vaalue, number of shared first four letters
    L = case_when(
      substr(NAME_ONE, 1, 1) != substr(NAME_TWO, 1, 1) ~ 0,
      substr(NAME_ONE, 1, 4) == substr(NAME_TWO, 1, 4) ~ 4,
      substr(NAME_ONE, 1, 3) == substr(NAME_TWO, 1, 3) ~ 3,
      substr(NAME_ONE, 1, 2) == substr(NAME_TWO, 1, 2) ~ 2,
      substr(NAME_ONE, 1, 1) == substr(NAME_TWO, 1, 1) ~ 1
    ),
    # 'Real' values impossible to be higher than approx value
    JW_APPROX = SIM + (L * 0.1 * (1 - SIM))
  ) %>%
  ungroup() %>%
  filter(JW_APPROX >= 0.75) %>%
  tally()










data <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT623_PDS_JW_FORENAMES"))

data <- data %>%
  mutate(ID = row_number(FORENAME_ONE)) %>%
  filter(ID >= 1000 & ID <= 2000)
#filter(FORENAME_ONE == "NATALIJA") %>%
#filter(FORENAME_TWO == "AURELIJA")

data

r <- data %>%
  mutate(
    TOKEN_ONE = trimws(REGEXP_REPLACE(FORENAME_ONE, '*', ' ')),
    S_ONE = nchar(FORENAME_ONE)
  ) %>%
  oracle_unnest_tokens(col = 'TOKEN_ONE') %>%
  group_by(ID, TOKEN) %>%
  mutate(TOKEN = paste0(TOKEN, rank(TOKEN_NUMBER))) %>%
  ungroup() %>%
  select(ID, FORENAME_ONE, S_ONE, TOKEN) %>%
  collect()

select(ID, FORENAME_ONE, TOKEN_ONE = TOKEN_NUMBER, TOKEN)

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
# Part Three: Implement manual JW

data <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT623_PDS_JW_FORENAMES"))

data <- data %>%
  mutate(ID = row_number(FORENAME_ONE))

forename_one <- data %>%
  mutate(
    TOKEN_ONE = trimws(REGEXP_REPLACE(FORENAME_ONE, '*', ' ')),
    S_ONE = nchar(FORENAME_ONE)
  ) %>%
  oracle_unnest_tokens(col = 'TOKEN_ONE') %>%
  group_by(ID, TOKEN) %>%
  mutate(TOKEN = paste0(TOKEN, rank(TOKEN_NUMBER))) %>%
  ungroup() %>%
  select(ID, FORENAME_ONE, S_ONE, TOKEN)

forename_two <- data %>%
  mutate(
    TOKEN_TWO = trimws(REGEXP_REPLACE(FORENAME_TWO, '*', ' ')),
    S_TWO = nchar(FORENAME_TWO)
  ) %>%
  oracle_unnest_tokens(col = 'TOKEN_TWO') %>%
  group_by(ID, TOKEN) %>%
  mutate(TOKEN = paste0(TOKEN, rank(TOKEN_NUMBER))) %>%
  ungroup() %>%
  select(ID, FORENAME_TWO, S_TWO, TOKEN)

Sys.time()

jw_approx <- forename_one %>%
  inner_join(forename_two) %>%
  group_by(ID) %>%
  mutate(
    M = n(),
    SIM = (1/3) * (((M / S_ONE) + (M / S_TWO) + ((M - 0) / M))),
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
  select(ID, FORENAME_ONE, FORENAME_TWO, JW) %>%
  distinct() %>%
  collect()

Sys.time()

z <- data %>%
  #filter(ID <= 10000) %>%
  mutate(JW = UTL_MATCH.JARO_WINKLER('FORENAME_ONE', 'FORENAME_TWO')) %>%
  select(ID, FORENAME_ONE, FORENAME_TWO, JW) %>%
  distinct() %>%
  collect()

#-------------------------------------------------------------------------------

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

data <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT623_PDS_JW_FORENAMES"))

data <- data %>%
  mutate(ID = row_number(FORENAME_ONE)) %>%
  filter(FORENAME_ONE == "NATALIJA") %>%
  filter(FORENAME_TWO == "AURELIJA")

a <- data %>%
  mutate(
    TOKEN_ONE = trimws(REGEXP_REPLACE(FORENAME_ONE, '*', ' ')),
    S_ONE = nchar(FORENAME_ONE)
  ) %>%
  oracle_unnest_tokens(col = 'TOKEN_ONE') %>%
  #group_by(ID, TOKEN) %>%
  #mutate(INDEX_ONE = rank(TOKEN_NUMBER)) %>%
  #ungroup() %>%
  select(ID, FORENAME_ONE, S_ONE, TOKEN_NUMBER_ONE = TOKEN_NUMBER, TOKEN)

b <- data %>%
  mutate(
    TOKEN_TWO = trimws(REGEXP_REPLACE(FORENAME_TWO, '*', ' ')),
    S_TWO = nchar(FORENAME_TWO)
  ) %>%
  oracle_unnest_tokens(col = 'TOKEN_TWO') %>%
  #group_by(ID, TOKEN) %>%
  #mutate(INDEX_TWO = rank(TOKEN_NUMBER)) %>%
  #ungroup() %>%
  select(ID, FORENAME_TWO, S_TWO, TOKEN_NUMBER_TWO = TOKEN_NUMBER, TOKEN)

#Sys.time()

d <- a %>%
  inner_join(b, by = c("ID", "TOKEN")) %>%
  group_by(ID) %>%
  mutate(
    MAX = ifelse(S_TWO >= S_ONE, S_TWO, S_ONE),
    LEN_DIST = floor((MAX / 2) - 1),
    CHAR_DIST = ifelse(abs(TOKEN_NUMBER_ONE - TOKEN_NUMBER_TWO) <= LEN_DIST, 1, 0)
  ) %>%
  ungroup(ID) %>%
  filter(CHAR_DIST == 1)

d

d %>%
  select(ID, TOKEN, FORENAME_TWO, S_TWO, TOKEN_NUMBER_TWO, LEN_DIST) %>%
  distinct() %>%
  group_by(ID, TOKEN) %>%
  mutate(
    TOKEN = paste0(TOKEN, rank(TOKEN_NUMBER_TWO)),
    SEARCH_TWO_START = ifelse(TOKEN_NUMBER_TWO - LEN_DIST <= 1, 1, TOKEN_NUMBER_TWO - LEN_DIST),
    SEARCH_TWO_END = ifelse(TOKEN_NUMBER_TWO + LEN_DIST >= S_TWO, S_TWO, TOKEN_NUMBER_TWO + LEN_DIST)
  ) %>%
  ungroup()

e

d %>%
  select(ID, TOKEN, FORENAME_ONE, S_ONE, TOKEN_NUMBER_ONE, LEN_DIST)
distinct() %>%
  group_by(ID, TOKEN) %>%
  mutate(
    TOKEN = paste0(TOKEN, rank(TOKEN_NUMBER_ONE)),
    SEARCH_ONE_START = ifelse(TOKEN_NUMBER_ONE - LEN_DIST <= 1, 1, TOKEN_NUMBER_ONE - LEN_DIST),
    SEARCH_ONE_END = ifelse(TOKEN_NUMBER_ONE + LEN_DIST >= S_ONE, S_ONE, TOKEN_NUMBER_ONE + LEN_DIST)
  ) %>%
  ungroup()

f

e %>%
  inner_join(f, by = c("ID", "TOKEN", "LEN_DIST")) %>%
  group_by(ID) %>%
  mutate(
    SEARCH_ONE_START = TOKEN_NUMBER_ONE - LEN_DIST
  )


group_by(ID) %>%
  mutate(
    M = n(),
    TOKEN_NUMBER_ONE = rank(TOKEN_NUMBER_ONE),
    TOKEN_NUMBER_TWO = rank(TOKEN_NUMBER_TWO),
    T_VAL = ifelse(TOKEN_NUMBER_ONE != TOKEN_NUMBER_TWO, 1, 0),
    T_VAL = sum(T_VAL) / 2,
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
  select(ID, FORENAME_ONE, FORENAME_TWO, JW) %>%
  distinct() %>%
  collect()

Sys.time()



