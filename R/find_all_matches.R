
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

df2 <- df2 %>%
  rename(
    ID_TWO = ID,
    FORENAME_TWO = FORENAME,
    SURNAME_TWO = SURNAME,
    DOB_TWO = DOB,
    POSTCODE_TWO = POSTCODE
  )


find_all_matches = function(
  df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
  df_two, id_two, forename_two, surname_two, dob_two, postcode_two
  ){

  # Initial format of data
  df_one <- df_one %>%
    dplyr::select(
      {{ id_one }},
      {{ forename_one }},
      {{ surname_one }},
      {{ dob_one }},
      {{ postcode_one }}
      ) %>%
    format_dob(df = ., dob = {{ dob_one }}) %>%
    format_name(df = ., name = {{ forename_one }}) %>%
    format_name(df = ., name = {{ surname_one }}) %>%
    format_postcode(., id = {{ id_one }}, postcode = {{ postcode_one }}) %>%
    calc_permutations(
      df = .,
      forename = {{ forename_one }},
      surname = {{ surname_one }},
      postcode = {{ postcode_one }},
      dob = {{ dob_one }}
      )

  # Initial format of data
  df_two <- df_two %>%
    dplyr::select(
      {{ id_two }},
      {{ forename_two }},
      {{ surname_two }},
      {{ dob_two }},
      {{ postcode_two }}
    ) %>%
    format_dob(df = ., dob = {{ dob_two }}) %>%
    format_name(df = ., name = {{ forename_two }}) %>%
    format_name(df = ., name = {{ surname_two }}) %>%
    format_postcode(., id = {{ id_two }}, postcode = {{ postcode_two }}) %>%
    calc_permutations(
      df = .,
      forename = {{ forename_two }},
      surname = {{ surname_two }},
      postcode = {{ postcode_two }},
      dob = {{ dob_two }}
    )

  # identify exact matches
  exact <- df_one %>%
    dplyr::select(
      {{ id_one }},
      {{ forename_one }},
      {{ surname_one }},
      {{ dob_one }},
      {{ postcode_one }}
    ) %>%
    # join where all fields match exactly
    dplyr::inner_join(
      y = df_two %>%
        dplyr::select(
          {{ id_two }},
          {{ forename_two }},
          {{ surname_two }},
          {{ dob_two }},
          {{ postcode_two }}
        ) %>%
        rename(
          {{ forename_one }} := {{ forename_two }},
          {{ surname_one }} := {{ surname_two }},
          {{ dob_one }} := {{ dob_two }},
          {{ postcode_one }} := {{ postcode_two }}
        ),
      keep = FALSE,
      na_matches = "never"
    ) %>%
    # Generate features for all exact matches
    # by default all matches are an "exact" match with perfect scores
    dplyr::mutate(
      {{ forename_two }} := {{ forename_one }},
      {{ surname_two }} := {{ surname_one }},
      {{ dob_two }} := {{ dob_one }},
      {{ postcode_two }} := {{ postcode_one }},
      JW_SURNAME = 1,
      JW_FORENAME = 1,
      JW_POSTCODE = 1,
      ED_DOB = 0,
      MATCH_TYPE = 'Exact'
    ) %>%
    dplyr::select({{ id_one }}, {{ id_two }}, everything())

  # Remove exact matches from primary df
  df_one <- df_one %>%
    dplyr::anti_join(y = exact %>% select({{ id_one }}))

  # Permutation join function
  perm_join <- function(df_one, df_two, id_one, id_two, perm_num){

    # Convert integer to permutation column name
    perm_num = sym(paste0("PERM", perm_num))

    # Join on composite value
    output <- dplyr::inner_join(
      x = df_one %>% dplyr::select({{ id_one }}, perm_num),
      y = df_two %>% dplyr::select({{ id_two }}, perm_num)
    )

    # Return output
    return(output)
  }

  # Lapply all permutations into list then row-bind
  id_pairs <- lapply(
    X = 1:9,
    FUN = perm_join,
    df_one = df_one,
    df_two = df_two,
    id_one = {{ id_one }},
    id_two = {{ id_two }}
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::select({{ id_one }}, {{ id_two }}) %>%
    dplyr::distinct()

  # Remove Permutation info from matching dfs
  df_one <- df_one %>%
    dplyr::select(
      {{ id_one }},
      {{ forename_one }},
      {{ surname_one }},
      {{ dob_one }},
      {{ postcode_one }}
    )

  # Remove Permutation info from matching dfs
  df_two <- df_two %>%
    dplyr::select(
      {{ id_two }},
      {{ forename_two }},
      {{ surname_two }},
      {{ dob_two }},
      {{ postcode_two }}
    )

  # Join lookup back to original dfs
  id_pairs <- id_pairs %>%
    dplyr::inner_join(y = df_one) %>%
    dplyr::inner_join(y = df_two)

  # Get list of eligible dobs and forenames
  eligible_dob_forename <- id_pairs %>%
    dplyr::select(
      {{ dob_one }},
      {{ dob_two }},
      {{ forename_one }},
      {{ forename_two }}
      ) %>%
    dplyr::mutate(
      ED_DOB = stringdist::stringdist(
        {{ dob_one }}, {{ dob_two }}, method = 'lv'
        ),
      JW_FORENAME = stringdist::stringsim(
        {{ forename_one }}, {{ forename_two }}, method = 'jw'
        )
    ) %>%
    dplyr::filter(ED_DOB <= 2 & JW_FORENAME >= 0.75) %>%
    distinct()

  # Filter permutation output by feasible date-pair list
  cross <- id_pairs %>%
    dplyr::inner_join(eligible_dob_forename)

  # Get Matches
  match <- cross %>%
    dplyr::mutate(
      JW_SURNAME = stringdist::stringsim(
        {{ surname_one }}, {{ surname_two }}, method = "jw"
        ),
      JW_POSTCODE = stringdist::stringsim(
        {{ postcode_one }}, {{ postcode_two }}, method = "jw"
        )
      ) %>%
      # limit to key fields and score matches
      dplyr::mutate(MATCH_TYPE = dplyr::case_when(
        JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB == 0 ~ "Exact",
        JW_SURNAME == 1 & JW_FORENAME == 1 & ED_DOB == 0 ~ "Confident",
        JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB <= 2 ~ "Confident",
        JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB == 0 ~ "Confident",
        JW_SURNAME == 1 & JW_FORENAME >= 0.75 & JW_POSTCODE == 1 & ED_DOB == 0 ~ "Confident",
        JW_SURNAME >= 0.85 & JW_FORENAME >= 0.75 & JW_POSTCODE >= 0.85 & ED_DOB <= 2 ~ "Confident",
        TRUE ~ "No Match"
        )
      ) %>%
      # filter to only confident matches
      dplyr::filter(MATCH_TYPE != "No Match")

  # combine the exact and confident matches
  matches <- match %>%
    rbind(exact)

  # return result
  return(matches)
}

output <- find_all_matches(
  df1, ID, FORENAME, SURNAME, DOB, POSTCODE,
  df2, ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO
)
