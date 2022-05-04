
# Library
library(dplyr)

# Functions
source("R/format_functions.R")

# Data
df1 = data.table::fread("C:/Users/ADNSH/Desktop/PAT_MATCH/DATA/TEST_DATA_10M.csv") %>%
  dplyr::sample_n(1000000)
gc()

# Data
df2 = data.table::fread("C:/Users/ADNSH/Desktop/PAT_MATCH/DATA/TEST_DATA_10M.csv") %>%
  dplyr::sample_n(1000000)
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
  format_data = FALSE
)



results %>%
  filter(
      stringr::str_detect(FORENAME, FORENAME_TWO) | stringr::str_detect(FORENAME_TWO, FORENAME)
    )
  )


results %>%
  mutate(
    INSTR = ifelse(
      stringr::str_detect(FORENAME, FORENAME_TWO) |
        stringr::str_detect(FORENAME_TWO, FORENAME), 1, 0
      )
    ) %>%
  filter(INSTR == 1) %>%
  select(-INSTR)
  filter(grepl(FORENAME, paste0(FORENAME_TWO, collapse = "|")))

stringr::str_detect("bi", "bhjnnbibh")
