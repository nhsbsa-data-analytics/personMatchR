# Library
library(dplyr)
library(dbplyr)

# Functions
source("R/calc_db_functions.R")

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db pds table
pds_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("STBUC", "INT617_TMP_PDS"))

# Db eibss table
eib_db <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("STBUC", "INT617_TMP_EIBSS"))

#-------------------------------------------------------------------------------
# Part One: Custom JW Calculation ~10m matches

# Just select forename
eib <- eib_db %>%
  select(FORENAME_ONE = FORENAME) %>%
  mutate(TMP = 1)

# Just select forename
pds <- pds_db %>%
  select(FORENAME_TWO = FORENAME) %>%
  mutate(
    TMP = 1,
    ID = row_number(FORENAME_TWO)
  ) %>%
  filter(ID <= 400000) %>%
  select(-ID)

# Full join
all <- eib %>%
  full_join(pds) %>%
  distinct()

# 10.7m distinct forename-pairs
all %>% tally()

# Time for 'normal' JW calculation: 10.7m = 36mins
Sys.time()
results_one <- all %>%
  mutate(JW = UTL_MATCH.JARO_WINKLER(FORENAME_ONE, FORENAME_TWO)) %>%
  filter(JW >= 0.75) %>%
  collect()
Sys.time()

# Time for custom JW calculation: 10.7m = 16mins
Sys.time()
results_two <- all %>%
  calc_db_jw_threshold(., FORENAME_ONE, FORENAME_TWO, 0.75, "JW") %>%
  collect()
Sys.time()

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
# Part Two: Custom JW function - Summary

# 1. Both methods have same output
# 2. 10.7m - Method 2 (in this instance) took ~44% of the time of Method 1
# 3. With smaller numbers, the difference is less
# 4. With very small number, diff will at some point become negligible
# 5. With larger numbers, difference may be more

#-------------------------------------------------------------------------------
