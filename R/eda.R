
# Library
library(dplyr)

# Functions
source("R/fix_homoglyph.R")
source("R/format_dob.R")
source("R/format_name.R")
source("R/format_postcode.R")

start_time = Sys.time()

# Data
df1 = read.csv("C:/Users/ADNSH/Desktop/TEST_DATA_1000000.csv") %>%
  sample_n(150000)

# Data
df2 = read.csv("C:/Users/ADNSH/Desktop/TEST_DATA_1000000.csv") %>%
  sample_n(150000)

# Initial format of data
df1 <- df1 %>%
  dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
  dplyr::mutate(
    SURNAME = format_name(SURNAME),
    FORENAME = format_name(FORENAME),
    DOB = purrr::map_chr(DOB, format_dob),
    POSTCODE = purrr::map_chr(POSTCODE, format_postcode)
  )

# Initial format of data
df2 <- df2 %>%
  dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
  dplyr::mutate(
    SURNAME = format_name(SURNAME),
    FORENAME = format_name(FORENAME),
    DOB = purrr::map_chr(DOB, format_dob),
    POSTCODE = purrr::map_chr(POSTCODE, format_postcode)
  )

# identify exact matches
df_exact <- df1 %>%
  # join where all fields match exactly
  dplyr::inner_join(
    y = df2,
    by = c(
      "SURNAME" = "SURNAME",
      "FORENAME" = "FORENAME",
      "DOB" = "DOB",
      "POSTCODE" = "POSTCODE"
    ),
    keep = FALSE,
    na_matches = "never"
  )

# Identify 'reverse name' exact matches
df_exact_rev <- df1 %>%
  # join where all fields match exactly
  dplyr::inner_join(
    y = df2,
    by = c(
      "FORENAME" = "SURNAME",
      "SURNAME" = "FORENAME",
      "DOB" = "DOB",
      "POSTCODE" = "POSTCODE"
    ),
    keep = FALSE,
    na_matches = "never"
  )

df_exact <- df_exact %>%
  dplyr::bind_rows(df_exact_rev) %>%
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

# Identify a vector of records where an exact match has been identified
exact_match_list <- df_exact %>%
  select(ID.x) %>%
  distinct() %>%
  pull()

# limit to only records not already matched based on an exact match
df_remain <- df1 %>%
  dplyr::filter(!ID %in% exact_match_list)

# Fuzzy Cross Join
cross_join = fuzzyjoin::stringdist_full_join(
    x = df_remain,
    y = df2,
    by = "FORENAME",
    method = "jw",
    max_dist = 0.15,
    ) %>%
  mutate(LV = stringdist::stringdist(DOB.x, DOB.y, "lv")) %>%
  filter(LV <= 2)

# Get Matches
matches <- cross_join %>%
  dplyr::mutate(
    JW_SURNAME = stringdist::stringsim(SURNAME.x, SURNAME.y, method = "jw"),
    JW_FORENAME = stringdist::stringsim(FORENAME.x, FORENAME.y, method = "jw"),
    JW_POSTCODE = stringdist::stringsim(POSTCODE.x, POSTCODE.y, method = "jw"),
    ED_DOB = stringdist::stringdist(DOB.x, DOB.y, method = "lv")
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

# Final Matches
matches <- matches %>%
  rbind(df_exact)

# Match Count
df_match_count <- matches %>%
  dplyr::group_by(ID.x) %>%
  dplyr::summarise(MATCH_COUNT = n()) %>%
  dplyr::ungroup()

# link the datasets back together
df_match_results <- matches %>%
  dplyr::inner_join(df_match_count, by = "ID.x") %>%
  dplyr::select(ID.x, MATCH_COUNT, ID.y, MATCH_TYPE)

end_time = Sys.time()

runtime = end_time - start_time

print(runtime)


z = df_remain %>%
  select(ID, FORENAME) %>%
  filter(row_number() <= 10000) %>%
  full_join(
    df2 %>%
      select(ID, FORENAME) %>%
      filter(row_number() <= 10000),
    by = character()
  ) %>%
  mutate(JW = stringdist::stringsim(FORENAME.x, FORENAME.y, "jw")) %>%
  filter(JW >= 0.85) %>%
  select(ID.x, ID.y)

y = df_remain %>%
  select(ID, POSTCODE) %>%
  filter(row_number() <= 10000) %>%
  full_join(
    df2 %>%
      select(ID, POSTCODE) %>%
      filter(row_number() <= 10000),
    by = character()
  ) %>%
  mutate(LV = stringdist::stringdist(POSTCODE.x, POSTCODE.y, "lv")) %>%
  filter(LV <= 2) %>%
  select(ID.x, ID.y)

rm(y)

inner_join(z,y)


chunk_num = ceiling(nrow(df_remain) / 100000)

df_remain %>%
  cbind(CHUNK = rep(1:chunk_num))











df3 <- df1 %>%
  # Filter rowid
  dplyr::filter(dplyr::row_number() == 100)

  # identify exact matches
  df_exact <- df3 %>%
  # Filter rowid
  # join where all fields match exactly
  dplyr::inner_join(
    y = df2,
    by = c(
      "SURNAME" = "SURNAME",
      "FORENAME" = "FORENAME",
      "DOB" = "DOB",
      "POSTCODE" = "POSTCODE"
    ),
    keep = FALSE,
    na_matches = "never"
  )

# Identify 'reverse name' exact matches
df_exact_rev <- df3 %>%
  # join where all fields match exactly
  dplyr::inner_join(
    y = df2,
    by = c(
      "FORENAME" = "SURNAME",
      "SURNAME" = "FORENAME",
      "DOB" = "DOB",
      "POSTCODE" = "POSTCODE"
    ),
    keep = FALSE,
    na_matches = "never"
  )

# Bind rows of both exact match df types
df_exact <- df_exact %>%
  dplyr::bind_rows(df_exact_rev) %>%
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

# Identify a vector of records where an exact match has been identified
exact_match_list <- df_exact %>%
  select(ID.x) %>%
  distinct() %>%
  pull()

# combine the two datasets (basic cross join) for non-exact matches records
df_combined <- df3 %>%
  # limit to only records not already matched based on an exact match
  dplyr::filter(!ID %in% exact_match_list) %>%
  # join the second dataset
  dplyr::full_join(df2, by = character(), suffix = c(".x", ".y")) %>%
  # perform string matching
  dplyr::mutate(
    JW_SURNAME = stringdist::stringsim(SURNAME.x, SURNAME.y, method = "jw"),
    JW_FORENAME = stringdist::stringsim(FORENAME.x, FORENAME.y, method = "jw"),
    JW_POSTCODE = stringdist::stringsim(POSTCODE.x, POSTCODE.y, method = "jw"),
    ED_DOB = stringdist::stringdist(DOB.x, DOB.y, method = "lv")
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

# combine the exact and confident matches
df_match_results <- rbind(df_exact, df_combined)

# identify the number of potential matches
df_match_count <- df_match_results %>%
  dplyr::group_by(ID.x) %>%
  dplyr::summarise(MATCH_COUNT = n()) %>%
  dplyr::ungroup()

# link the datasets back together
df_match_results <- df_match_results %>%
  dplyr::inner_join(df_match_count, by = "ID.x") %>%
  dplyr::select(ID.x, MATCH_COUNT, ID.y, MATCH_TYPE)


















# 1 record matched against 100k Records takes 116s
start_time = Sys.time()
edit_find_matches(df1 %>% filter(dplyr::row_number() == 5), df1)
end_time = Sys.time()
take_time = end_time - start_time
print(take_time)



df1 = data %>%
  mutate(
    FN_FIRST_CHAR = substr(FORENAME, 1, 1),
  )

# Select and format relevant fields: ID, SURNAME, FORENAME, DOB, POSTCODE
df1 <- df1 %>%
  dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
  dplyr::mutate(
    SURNAME = format_name(SURNAME),
    FORENAME = format_name(FORENAME),
    DOB = purrr::map_chr(DOB, format_dob),
    POSTCODE = purrr::map_chr(POSTCODE, format_postcode)
  )

# Select and format relevant fields: ID, SURNAME, FORENAME, DOB, POSTCODE
df2 <- df2 %>%
  dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
  dplyr::mutate(
    SURNAME = format_name(SURNAME),
    FORENAME = format_name(FORENAME),
    DOB = purrr::map_chr(DOB, format_dob),
    POSTCODE = purrr::map_chr(POSTCODE, format_postcode)
  )

# identify exact matches
df_exact <- df1 %>%
  # join where all fields match exactly
  dplyr::inner_join(
    y = df2,
    by = c(
      "SURNAME" = "SURNAME",
      "FORENAME" = "FORENAME",
      "DOB" = "DOB",
      "POSTCODE" = "POSTCODE"
    ),
    keep = FALSE,
    na_matches = "never"
  )

# Identify 'reverse name' exact matches
df_exact_rev <- df1 %>%
  # join where all fields match exactly
  dplyr::inner_join(
    y = df2,
    by = c(
      "FORENAME" = "SURNAME",
      "SURNAME" = "FORENAME",
      "DOB" = "DOB",
      "POSTCODE" = "POSTCODE"
    ),
    keep = FALSE,
    na_matches = "never"
  )

# Bind rows of both exact match df types
df_exact <- df_exact %>%
  dplyr::bind_rows(df_exact_rev) %>%
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

# Identify a vector of records where an exact match has been identified
exact_match_list <- df_exact %>%
  select(ID.x) %>%
  distinct() %>%
  pull()

# combine the two datasets (basic cross join) for non-exact matches records
df_combined <- df1 %>%
  # limit to only records not already matched based on an exact match
  dplyr::filter(!ID %in% exact_match_list) %>%
  # join the second dataset
  dplyr::full_join(df2, by = character(), suffix = c(".x", ".y")) %>%
  # perform string matching
  dplyr::mutate(
    JW_SURNAME = stringdist::stringsim(SURNAME.x, SURNAME.y, method = "jw"),
    JW_FORENAME = stringdist::stringsim(FORENAME.x, FORENAME.y, method = "jw"),
    JW_POSTCODE = stringdist::stringsim(POSTCODE.x, POSTCODE.y, method = "jw"),
    ED_DOB = stringdist::stringdist(DOB.x, DOB.y, method = "lv")
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

# combine the exact and confident matches
df_match_results <- rbind(df_exact, df_combined)

# identify the number of potential matches
df_match_count <- df_match_results %>%
  dplyr::group_by(ID.x) %>%
  dplyr::summarise(MATCH_COUNT = n()) %>%
  dplyr::ungroup()

# link the datasets back together
df_match_results <- df_match_results %>%
  dplyr::inner_join(df_match_count, by = "ID.x") %>%
  dplyr::select(ID.x, MATCH_COUNT, ID.y, MATCH_TYPE)
