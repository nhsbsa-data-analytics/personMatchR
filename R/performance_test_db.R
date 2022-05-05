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


# Part One: Custom DOB Distance Calculation ~10m matches: Setup ----------------

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

# Part One: Custom DOB Distance Calculation ~10m matches: Test ----------------

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
  filter_dob_db(., DOB_ONE, DOB_TWO, 2) %>%
  collect()
Sys.time()

# Disconnect
DBI::dbDisconnect(con)


