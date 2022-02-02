
# Library
library(dplyr)

# Functions
source("R/format_functions.R")

# Data
df1 = read.csv("C:/Users/ADNSH/Desktop/TEST_DATA_1000000.csv") %>%
  sample_n(100000)

# Data
df2 = read.csv("C:/Users/ADNSH/Desktop/TEST_DATA_1000000.csv") %>%
  sample_n(100000)


find_all_matches = function(df1, df2){

  # Function Start
  print(paste0("Cleaning Start: ", Sys.time()))

  # Initial format of data
  df1 <- df1 %>%
    dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
    format_dob() %>%
    format_name() %>%
    format_postcode()

  # Initial format of data
  df2 <- df2 %>%
    dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
    format_dob() %>%
    format_name() %>%
    format_postcode()

  # Function Start
  print(paste0("Cleaning End: ", Sys.time()))

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

  # Generate features for all exact matches
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
  df1 <- df1 %>%
    dplyr::filter(!ID %in% exact_match_list)

  # Vector of values for lapply
  chunk_size <- 2000000000 / nrow(df2)
  chunk_num <- ceiling(nrow(df1) / chunk_size)
  chunk_vec = rep(1:chunk_num, chunk_size)[1:nrow(df1)]

  # Add Chunk Variable to Remaining DF
  df1 <- df1 %>%
    cbind(CHUNK = chunk_vec)

  # Function for lapply
  cross_match <- function(df1, df2, chunk){

    # Filter One of Pre-Defined Subsets
    df1 <- df1 %>%
      dplyr::filter(CHUNK == chunk) %>%
      dplyr::select(-CHUNK)

    # Fuzzy Cross Join
    cross_join = fuzzyjoin::stringdist_full_join(
      x = df1,
      y = df2,
      by = "FORENAME",
      method = "jw",
      max_dist = 0.25
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

    # Clean then return
    gc()
    return(matches)
  }

  # Function Start
  print(paste0("Cross Match Start: ", Sys.time()))

  # Get lapply results and bind rows
  matches <- lapply(X = 1:chunk_num, FUN = cross_match, df1 = df1, df2 = df2) %>%
    dplyr::bind_rows()

  # Function End
  print(paste0("Cross Match End: ", Sys.time()))

  # combine the exact and confident matches
  matches <- matches %>%
    rbind(df_exact) %>%
    dplyr::group_by(ID.x) %>%
    dplyr::mutate(MATCH_COUNT = n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(ID.x, MATCH_COUNT, ID.y, MATCH_TYPE)

  # return result
  return(matches)
}

output <- find_all_matches(df1, df2)
