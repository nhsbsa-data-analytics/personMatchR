
# Library
library(dplyr)

# Functions
source("R/format_functions.R")

# Data
df1 = data.table::fread("C:/Users/ADNSH/Desktop/PAT_MATCH/DATA/TEST_DATA_10M.csv") %>%
  dplyr::sample_n(100000)
gc()

# Data
df2 = data.table::fread("C:/Users/ADNSH/Desktop/PAT_MATCH/DATA/TEST_DATA_10M.csv") %>%
  dplyr::sample_n(100000)
gc()

df2 <- df2 %>%
  dplyr::rename(
    ID_TWO = ID,
    FORENAME_TWO = FORENAME,
    SURNAME_TWO = SURNAME,
    DOB_TWO = DOB,
    POSTCODE_TWO = POSTCODE
  )

results <- find_matches(
  df1, ID, FORENAME, SURNAME, DOB, POSTCODE,
  df2, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO,
  output_type = "all",
  format_data = T
)
