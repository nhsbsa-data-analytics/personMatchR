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
  distinct() %>%
  mutate(TMP = 1)

# Just select forename
pds <- pds_db %>%
  filter(RECORD_ID <= 10) %>%
  select(FORENAME_TWO = FORENAME) %>%
  distinct() %>%
  mutate(TMP = 1)

# Full join
all <- eib %>%
  full_join(pds) %>%
  mutate(ID = row_number(TMP))

# Row count
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
  calc_db_jw_threshold_edit(., FORENAME_ONE, FORENAME_TWO, 0.75, "JW") %>%
  collect()
Sys.time()

# Disconnect
DBI::dbDisconnect(con)

results_two %>%
  select(FORENAME_ONE, FORENAME_TWO) %>%
  distinct() %>%
  tally()

#-------------------------------------------------------------------------------
# Part Two: Custom JW function - Summary

# 1. Both methods have same output
# 2. 10.7m - Method 2 (in this instance) took ~44% of the time of Method 1
# 3. With smaller numbers, the difference is less
# 4. With very small number, diff will at some point become negligible
# 5. With larger numbers, difference may be more

#-------------------------------------------------------------------------------
# Part Three: Custom DOB Distance Calculation ~10m matches

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Just select dob
eib <- eib_db %>%
  select(DOB_ONE = DOB) %>%
  mutate(TMP = 1)

# Just select dob
pds <- pds_db %>%
  select(DOB_TWO = DOB) %>%
  mutate(TMP = 1) %>%
  distinct()

# Full join
all <- eib %>%
  full_join(pds) %>%
  distinct()

# Write the table back to the DB with indexes
all %>%
  compute(
    name = "INT623_DOB_TEST",
    temporary = FALSE
  )

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
# Part One: Custom JW Calculation ~10m matches

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Db eibss table
all <- con %>%
  dplyr::tbl(from = dbplyr::in_schema("ADNSH", "INT623_DOB_TEST"))

# 10.1m distinct forename-pairs
all %>% tally()

# Time for 'normal' LV calculation: 10.1m = 8mins 33s (513s)
Sys.time()
results_one <- all %>%
  mutate(LV = UTL_MATCH.EDIT_DISTANCE(DOB_ONE, DOB_TWO)) %>%
  filter(LV <= 2) %>%
  collect()
Sys.time()

# Time for custom DOB-Dist calculation: 10.1m = 3mins 4s (184s)
Sys.time()
results_two <- all %>%
  dob_lv_filter(., DOB_ONE, DOB_TWO) %>%
  collect()
Sys.time()

# Disconnect
DBI::dbDisconnect(con)

#-------------------------------------------------------------------------------
# Part Four: Custom Date-Dist function - Summary

# 1. There are less dob-combinaations that name-combinations
# 2. Date-Dist is similar, although *not* identical, to LV, thus outputs vary
# 3. Date-dist in effect is 6 identical characters
# 4. LV dist of 2 can instances or 5 (or even 4) identical chars, due to swaps
# 5. Date-Dist is a 'tighter' control  as a result
# 6. Date-Dist took 184s for 10.1m dob-pairs
# 7. LV took 513s for 10.1m dob-pairs
# 8. Date-Dist took 36% of the time that LV did

#-------------------------------------------------------------------------------
