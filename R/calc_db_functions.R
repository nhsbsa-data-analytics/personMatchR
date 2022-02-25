#' Applies all combinations of dob_substr() filters
#'
#' Through doing so, it mirrors an edit distance of 2 on an 8-digit string
#' For this very particular use case, is faster than LV within SQL
#'
#' @param df A df with dates to be formatted
#'
#' @return A df with relevant dates from both columns
#'
#' @export
#'
#' @examples
#' dob_substr(df, sub_one, sub_two, sub_three)
dob_lv_filter <- function(df, dob_one, dob_two){

  # Rename df for convenience
  df <- df %>%
    dplyr::select({{ dob_one }}, {{ dob_two }}) %>%
    dplyr::distinct()

  # Function to apply multiple times
  dob_substr <- function(df, sub_one, sub_two, sub_three){

    df %>%
      dplyr::filter(
        SUBSTR({{ dob_one }}, sub_one, 1) == SUBSTR({{ dob_two }}, sub_one, 1) |
          # Tokens share same second letter
          SUBSTR({{ dob_one }}, sub_two, 1) == SUBSTR({{ dob_two }}, sub_two, 1) |
          # Tokens share same second letter
          SUBSTR({{ dob_one }}, sub_three, 1) == SUBSTR({{ dob_two }}, sub_three, 1)
      )
    }

  # Apply multiple filters to replicate use case of LV
  df <- df %>%
    dob_substr(., 1,2,3) %>%
    dob_substr(., 1,2,4) %>%
    dob_substr(., 1,2,5) %>%
    dob_substr(., 1,2,6) %>%
    dob_substr(., 1,2,7) %>%
    dob_substr(., 1,2,8) %>%
    dob_substr(., 1,3,4) %>%
    dob_substr(., 1,3,5) %>%
    dob_substr(., 1,3,6) %>%
    dob_substr(., 1,3,7) %>%
    dob_substr(., 1,3,8) %>%
    dob_substr(., 1,4,5) %>%
    dob_substr(., 1,4,6) %>%
    dob_substr(., 1,4,7) %>%
    dob_substr(., 1,4,8) %>%
    dob_substr(., 1,5,6) %>%
    dob_substr(., 1,5,7) %>%
    dob_substr(., 1,5,8) %>%
    dob_substr(., 1,6,7) %>%
    dob_substr(., 1,6,8) %>%
    dob_substr(., 1,7,8) %>%
    dob_substr(., 2,3,4) %>%
    dob_substr(., 2,3,5) %>%
    dob_substr(., 2,3,6) %>%
    dob_substr(., 2,3,7) %>%
    dob_substr(., 2,3,8) %>%
    dob_substr(., 2,4,5) %>%
    dob_substr(., 2,4,6) %>%
    dob_substr(., 2,4,7) %>%
    dob_substr(., 2,4,8) %>%
    dob_substr(., 2,5,6) %>%
    dob_substr(., 2,5,7) %>%
    dob_substr(., 2,5,8) %>%
    dob_substr(., 2,6,7) %>%
    dob_substr(., 2,6,8) %>%
    dob_substr(., 2,7,8) %>%
    dob_substr(., 3,4,5) %>%
    dob_substr(., 3,4,6) %>%
    dob_substr(., 3,4,7) %>%
    dob_substr(., 3,4,8) %>%
    dob_substr(., 3,5,6) %>%
    dob_substr(., 3,5,7) %>%
    dob_substr(., 3,5,8) %>%
    dob_substr(., 3,6,7) %>%
    dob_substr(., 3,6,8) %>%
    dob_substr(., 3,7,8) %>%
    dob_substr(., 4,5,6) %>%
    dob_substr(., 4,5,7) %>%
    dob_substr(., 4,5,8) %>%
    dob_substr(., 4,6,7) %>%
    dob_substr(., 4,6,8) %>%
    dob_substr(., 4,7,8) %>%
    dob_substr(., 5,6,7) %>%
    dob_substr(., 5,6,8) %>%
    dob_substr(., 5,7,8) %>%
    dob_substr(., 6,7,8)

  # Return
  return(df)
}

#' Applies a filter on either forename or surname to limit cross-join
#'
#' Only contains name-pair instances that share certain characteristsics
#' These include same 1st, 2nd or last letter, or being a substring of another
#'
#' @param df A df to be formatted
#' @param name_one first name column
#' @param name_two second name column
#'
#' @return A df with a filtered name-col to limit post cross-join
#'
#' @export
#'
#' @examples
#' name_db_filter(df, name_one, name_two)
name_db_filter <- function(df, name_one, name_two){

  df <- df %>%
    dplyr::filter(
      # Tokens share the same first letter
      SUBSTR({{ name_one  }}, 1, 1) == SUBSTR({{ name_two  }}, 1, 1) |
        # Tokens share same second letter
        SUBSTR({{ name_one  }}, 2, 1) == SUBSTR({{ name_two  }}, 2, 1) |
        # Tokens share same last letter
        SUBSTR({{ name_one  }}, LENGTH({{ name_one  }}), 1) == SUBSTR({{ name_two  }}, LENGTH({{ name_two  }}), 1) |
        # One token is a substring of the other
        INSTR({{ name_one  }}, {{ name_two  }}) > 1 |
        INSTR({{ name_two  }}, {{ name_one  }}) > 1
    )
  }

#' Calculates 9 permutations for primary-lookup join prior to match scoring
#'
#' @param df A df to be formatted
#' @param forename forename column name
#' @param surname surname column name
#' @param postcode postcode column name
#' @param dob DOB column name
#'
#' @return A df with 5 'basic' join permutations added
#'
#' @export
#'
#' @examples
#' calc_permutations(df, forename, surname, postcode, dob)
calc_permutations <- function(df, forename, surname, postcode, dob){

  df %>%
    dplyr::mutate(
      # Perm 1-3 require full match of dob with 2 of forename, surname, postcode
      PERM1 = paste0({{ forename }}, {{ surname }}, {{ dob }}),
      PERM2 = paste0({{ forename }}, {{ postcode }}, {{dob}}),
      PERM3 = paste0({{ surname }}, {{ postcode }}, {{ dob }}),
      # First char forename, plus 4 chars of surname and postcode
      PERM4 = paste0(
        substr({{ forename }}, 1, 1),
        substr({{ surname }}, 1, 4),
        substr({{ postcode }}, 1, 4)
        ),
      # First 3 chars forename - 2 chars of surname & postcode
      PERM5 = paste0(
        substr({{ forename }}, 1, 3),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # Last 3 chars forename - 3 chars of surname & postcode
      PERM6 = paste0(
        SUBSTR({{ forename }}, nchar({{ forename }})-2, 3),
        substr({{ surname }}, 1, 3),
        substr({{ postcode }}, 1, 3)
      ),
      # First 3 consonants - 3 chars of surname & postcode
      PERM7 = paste0(
        substr(REGEXP_REPLACE({{ forename}}, '[AEIOU]', ''), 1, 3),
        substr({{ surname }}, 1, 3),
        substr({{ postcode }}, 1, 3)
      ),
      # All consonants - 2 chars of surname & postcode
      PERM8 = paste0(
        REGEXP_REPLACE({{ forename}}, '[AEIOU]', ''),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # All vowels - 3 chars of surname & postcode
      PERM9 = paste0(
        REGEXP_REPLACE({{ forename }}, '[B-DF-HJ-NP-TV-Z]', ''),
        substr({{ surname }}, 1, 3),
        substr({{ postcode }}, 1, 3)
      )
    )
}

#' Calculates two strings JW score, using a JW approximation to limit results
#'
#' JW can be slow within SQL Developer
#' A quick JW-approximation can limit results before then applying this
#' Scores below threshold not returned
#'
#' @param df A df to be formatted
#' @param name_one first name column
#' @param name_two second name column
#' @param threshold_val retain only records with JW of a certain score or higher
#' @param col_name what the name of the new output JW-scored column should be
#'
#' @return A df only with name-pairs with a JW value above a threshold
#'
#' @export
#'
#' @examples
#' calc_db_jw_threshold(df, name_one, name_two, threshold_val)
calc_db_jw_threshold <- function(df, name_one, name_two, threshold_val, col_name){

  # Distinct df to calculate JW scores for
  output <- df %>%
    dplyr::rename(
      NAME_ONE := {{ name_one }},
      NAME_TWO := {{ name_two }}
    ) %>%
    dplyr::distinct() %>%
    #name_db_filter(., NAME_ONE, NAME_TWO) %>%
    dplyr::mutate(ID = row_number(NAME_ONE))

  # Pull the connection
  db_connection <- output$src$con

  # Formulate SQL query one, to split on character
  sql_query_one <- dbplyr::build_sql(
    con = db_connection,
    "
  SELECT
    id, name_one,
    token || row_number() over (partition by id, name_one, token order by id, name_one, token)  as  token
    FROM  (", dbplyr::sql_render(output), ")  t
    CROSS JOIN LATERAL (
    SELECT SUBSTR(t.name_one, LEVEL, 1) as token FROM DUAL CONNECT BY LEVEL <= LENGTH(t.name_one)
    )
  "
  )

  # Formulate SQL query one, to split on character
  sql_query_two <- dbplyr::build_sql(
    con = db_connection,
    "
  SELECT
    id, name_two,
    token || row_number() over (partition by id, name_two, token order by id, name_two, token)  as  token
    FROM  (", dbplyr::sql_render(output), ")  t
    CROSS JOIN LATERAL (
    SELECT SUBSTR(t.name_two, LEVEL, 1) as token FROM DUAL CONNECT BY LEVEL <= LENGTH(t.name_two)
    )
  "
  )

  # Join two SQL outputs
  output <- inner_join(
    x = dplyr::tbl(db_connection, dplyr::sql(sql_query_one)),
    y = dplyr::tbl(db_connection, dplyr::sql(sql_query_two))
    )

  # Generate SIM-approx
  output <- output %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      # NOTE: char-distance is ignored, as only approx calculation
      M = n(),
      # Assume max transpositions to speed up approx calculation
      SIM = (1/3) * ( (M / nchar(NAME_ONE)) + (M / nchar(NAME_TWO)) + 1 ),
      # Final JW value, number of shared first four letters
      L = case_when(
        substr(NAME_ONE, 1, 4) == substr(NAME_TWO, 1, 4) ~ 4,
        substr(NAME_ONE, 1, 3) == substr(NAME_TWO, 1, 3) ~ 3,
        substr(NAME_ONE, 1, 2) == substr(NAME_TWO, 1, 2) ~ 2,
        T ~ 1
      ),
      # 'Real' values impossible to be higher than approx value
      JW_APPROX = SIM + (L * 0.1 * (1 - SIM))
      #JW_APPROX = SIM + (4 * 0.1 * (1 - SIM))
    ) %>%
    dplyr::ungroup() %>%
    # Filter by threshold value
    dplyr::filter(JW_APPROX >= threshold_val) %>%
    dplyr::select(NAME_ONE, NAME_TWO, JW_APPROX) %>%
    dplyr::distinct() %>%
    dplyr::mutate(JW_APPROX = UTL_MATCH.JARO_WINKLER(NAME_ONE, NAME_TWO)) %>%
    dplyr::filter(JW_APPROX >= threshold_val) %>%
    dplyr::rename_at("JW_APPROX", ~col_name) %>%
    dplyr::rename(
      {{ name_one }} := NAME_ONE,
      {{ name_two }} := NAME_TWO
    )

  # Return output
  return(output)
}

#' Function to find all matches form the db
#'
#' Find the exact and confident matches of a lookup df to a primary df
#'
#' @param df_one The primary df
#' @param df_two The lookup df (being matched against)
#' @param id_one primary df unique identifier
#' @param forename_one primary df forename
#' @param surname_one primary df surname
#' @param dob_one primary df DOB
#' @param postcode_one primary df postcode
#' @param id_two lookup df unique identifier
#' @param forename_two lookup df forename
#' @param surname_two lookup df surname
#' @param dob_two lookup df DOB
#' @param postcode_two lookup df postcode
#'
#' @return The primary df matched against the lookup df
#' @return This will a combinaation of exact, confident and non-matches
#'
#' @export
#'
#' @examples
#' find_db_matches(
#' df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
#' df_two, id_two, forename_two, surname_two, dob_two, postcode_two)
find_db_matches_one <- function(
  df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
  df_two, id_two, forename_two, surname_two, dob_two, postcode_two
){

  # For convenience rename columns
  df_one <- df_one %>%
    rename(
      ID_ONE := {{ id_one }},
      FORENAME_ONE := {{ forename_one }},
      SURNAME_ONE := {{ surname_one }},
      DOB_ONE := {{ dob_one }},
      POSTCODE_ONE := {{ postcode_one }}
    )

  # For convenience rename columns
  df_two <- df_two %>%
    rename(
      ID_TWO := {{ id_two }},
      FORENAME_TWO := {{ forename_two }},
      SURNAME_TWO := {{ surname_two }},
      DOB_TWO := {{ dob_two }},
      POSTCODE_TWO := {{ postcode_two }}
    )

  # Exact matches
  exact <- df_one %>%
    dplyr::select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE) %>%
    dplyr::inner_join(
      y = df_two %>%
        dplyr::select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO),
      by = c(
        "FORENAME_ONE" = "FORENAME_TWO",
        "SURNAME_ONE" = "SURNAME_TWO",
        "DOB_ONE" = "DOB_TWO",
        "POSTCODE_ONE" = "POSTCODE_TWO"
      )
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      FORENAME_TWO = FORENAME_ONE,
      SURNAME_TWO = SURNAME_ONE,
      DOB_TWO = DOB_ONE,
      POSTCODE_TWO = POSTCODE_ONE,
      JW_FORENAME = 1,
      JW_SURNAME = 1,
      JW_POSTCODE = 1,
      ED_DOB = 0,
      MATCH_TYPE = 'Exact',
    ) %>%
    dplyr::select(ID_ONE, ID_TWO, everything())

  # Remaining records
  remain <- df_one %>%
    dplyr::anti_join(y = exact, by = "ID_ONE")

  # List of permutation-join columns
  perm_num <- paste0("PERM", 1:9)

  # Distinct list of ID perm-join pairs
  id_pairs <- perm_num %>%
    purrr::map(~{

      remain %>%
        dplyr::select(ID_ONE, {{.x}}) %>%
        dplyr::inner_join(
          df_two %>%
            dplyr::select(ID_TWO, {{.x}})
        ) %>%
        dplyr::select(ID_ONE, ID_TWO)
    }) %>%
    purrr::reduce(function(x, y) union(x, y)) %>%
    dplyr::distinct()

  # Remove Permutation info from matching dfs
  df_one <- df_one %>%
    dplyr::select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE)

  # Remove Permutation info from matching dfs
  df_two <- df_two %>%
    dplyr::select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO)

  # Join lookup back to original dfs
  id_pairs <- id_pairs %>%
    dplyr::inner_join(y = df_one, by = "ID_ONE") %>%
    dplyr::inner_join(y = df_two, by = "ID_TWO")

  # Generate list of feasible dob-pairs with 6 identical characters
  eligible_dates <- dob_lv_filter(
    df = id_pairs,
    dob_one = DOB_ONE,
    dob_two = DOB_TWO
  )

  # Generate a list of feasible forename pairs
  eligible_names <- calc_db_jw_threshold(
    df = id_pairs,
    name_one = FORENAME_ONE,
    name_two = FORENAME_TWO,
    threshold_val = 0.75,
    col_name = 'JW_FORENAME'
  )

  # Filter permutation output by feasible date-pair list
  cross <- id_pairs %>%
    dplyr::inner_join(eligible_dates) %>%
    dplyr::inner_join(eligible_names)

  # Generate a list
  match <- cross %>%
    dplyr::mutate(
      # NAs for zeros
      JW_SURNAME = UTL_MATCH.JARO_WINKLER(SURNAME_ONE, SURNAME_TWO),
      JW_POSTCODE = UTL_MATCH.JARO_WINKLER(POSTCODE_ONE, POSTCODE_TWO),
      ED_DOB = ifelse(DOB_ONE == DOB_TWO, 0, 2),
      # Generate confident matches
      MATCH_TYPE = dplyr::case_when(
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

  # Final Results
  match <- match %>%
    dplyr::union_all(exact)

  # Determine non-matches
  non_matches <- df_one %>%
    dplyr::anti_join(y = match %>% dplyr::select(ID_ONE)) %>%
    dplyr::mutate(
      ID_TWO = NA,
      FORENAME_TWO = NA,
      SURNAME_TWO= NA,
      DOB_TWO = NA,
      POSTCODE_TWO = NA,
      JW_SURNAME = NA,
      JW_FORENAME = NA,
      JW_POSTCODE = NA,
      ED_DOB = NA,
      MATCH_TYPE = "No Match"
    )

  # Add non-matches
  all_match <- match %>%
    dplyr::union_all(non_matches) %>%
    # Calculate match_count per primary dfID
    dplyr::group_by(ID_ONE) %>%
    dplyr::mutate(MATCH_COUNT = dplyr::n_distinct(ID_TWO)) %>%
    dplyr::ungroup() %>%
    # Rename back to OG column names
    dplyr::rename(
      {{ id_one }} := ID_ONE,
      {{ forename_one }} := FORENAME_ONE,
      {{ surname_one }} := SURNAME_ONE,
      {{ dob_one }} := DOB_ONE,
      {{ postcode_one }} := POSTCODE_ONE,
      {{ id_two }} := ID_TWO,
      {{ forename_two }} := FORENAME_TWO,
      {{ surname_two }} := SURNAME_ONE,
      {{ dob_two }} := DOB_TWO,
      {{ postcode_two }} := POSTCODE_TWO
    )

  # Return data
  return(all_match)
}

#' Function to find all matches form the db
#'
#' Find the exact and confident matches of a lookup df to a primary df
#'
#' @param df_one The primary df
#' @param df_two The lookup df (being matched against)
#' @param id_one primary df unique identifier
#' @param forename_one primary df forename
#' @param surname_one primary df surname
#' @param dob_one primary df DOB
#' @param postcode_one primary df postcode
#' @param id_two lookup df unique identifier
#' @param forename_two lookup df forename
#' @param surname_two lookup df surname
#' @param dob_two lookup df DOB
#' @param postcode_two lookup df postcode
#'
#' @return The primary df matched against the lookup df
#' @return This will a combinaation of exact, confident and non-matches
#'
#' @export
#'
#' @examples
#' find_db_matches(
#' df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
#' df_two, id_two, forename_two, surname_two, dob_two, postcode_two)
find_db_matches_two <- function(
  df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
  df_two, id_two, forename_two, surname_two, dob_two, postcode_two
  ){

  # For convenience rename columns
  df_one <- df_one %>%
    rename(
      ID_ONE := {{ id_one }},
      FORENAME_ONE := {{ forename_one }},
      SURNAME_ONE := {{ surname_one }},
      DOB_ONE := {{ dob_one }},
      POSTCODE_ONE := {{ postcode_one }}
    )

  # For convenience rename columns
  df_two <- df_two %>%
    rename(
      ID_TWO := {{ id_two }},
      FORENAME_TWO := {{ forename_two }},
      SURNAME_TWO := {{ surname_two }},
      DOB_TWO := {{ dob_two }},
      POSTCODE_TWO := {{ postcode_two }}
    )

  # Exact matches
  exact <- df_one %>%
    dplyr::select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE) %>%
    dplyr::inner_join(
      y = df_two %>%
        dplyr::select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO),
      by = c(
        "FORENAME_ONE" = "FORENAME_TWO",
        "SURNAME_ONE" = "SURNAME_TWO",
        "DOB_ONE" = "DOB_TWO",
        "POSTCODE_ONE" = "POSTCODE_TWO"
      )
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      FORENAME_TWO = FORENAME_ONE,
      SURNAME_TWO = SURNAME_ONE,
      DOB_TWO = DOB_ONE,
      POSTCODE_TWO = POSTCODE_ONE,
      JW_FORENAME = 1,
      JW_SURNAME = 1,
      JW_POSTCODE = 1,
      ED_DOB = 0,
      MATCH_TYPE = 'Exact',
    ) %>%
    dplyr::select(ID_ONE, ID_TWO, everything())

  # Remaining records
  remain <- df_one %>%
    dplyr::anti_join(y = exact, by = "ID_ONE")

  # List of permutation-join columns
  perm_num <- paste0("PERM", 1:9)

  # Distinct list of ID perm-join pairs
  id_pairs <- perm_num %>%
    purrr::map(~{

      remain %>%
        dplyr::select(ID_ONE, {{.x}}) %>%
        dplyr::inner_join(
          df_two %>%
            dplyr::select(ID_TWO, {{.x}})
        ) %>%
        dplyr::select(ID_ONE, ID_TWO)
    }) %>%
    purrr::reduce(function(x, y) union(x, y)) %>%
    dplyr::distinct()

  # Remove Permutation info from matching dfs
  df_one <- df_one %>%
    dplyr::select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE)

  # Remove Permutation info from matching dfs
  df_two <- df_two %>%
    dplyr::select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO)

  # Join lookup back to original dfs
  id_pairs <- id_pairs %>%
    dplyr::inner_join(y = df_one, by = "ID_ONE") %>%
    dplyr::inner_join(y = df_two, by = "ID_TWO")

  # Generate list of feasible dob-pairs with 6 identical characters
  eligible_dates <- dob_lv_filter(
    df = id_pairs,
    dob_one = DOB_ONE,
    dob_two = DOB_TWO
  )

  # Filter permutation output by feasible date-pair list
  cross <- id_pairs %>%
     dplyr::inner_join(eligible_dates)

  # Generate a list of feasible forename pairs
  eligible_names <- calc_db_jw_threshold(
     df = cross,
     name_one = FORENAME_ONE,
     name_two = FORENAME_TWO,
     threshold_val = 0.75,
     col_name = 'JW_FORENAME'
   )

  # Filter permutation output by feasible date-pair list
  cross <- cross %>%
     dplyr::inner_join(eligible_names)

  # Get JW scores for surname
  surname_jw_score <- calc_db_jw_threshold(
     df = cross,
     name_one = SURNAME_ONE,
     name_two = SURNAME_TWO,
     threshold_val = 0.85,
     col_name = 'JW_SURNAME'
  )

  # Get JW scores for postcode
  postcode_jw_score <- calc_db_jw_threshold(
    df = cross,
    name_one = POSTCODE_ONE,
    name_two = POSTCODE_TWO,
    threshold_val = 0.85,
    col_name = 'JW_POSTCODE'
  )

  # Generate a list
  match <- cross %>%
    dplyr::left_join(y = surname_jw_score) %>%
    dplyr::left_join(y = postcode_jw_score) %>%
    dplyr::mutate(
      # NAs for zeros
      JW_SURNAME = ifelse(!is.na(JW_SURNAME), JW_SURNAME, 0),
      JW_POSTCODE = ifelse(!is.na(JW_POSTCODE), JW_POSTCODE, 0),
      ED_DOB = ifelse(DOB_ONE == DOB_TWO, 0, 2),
      # Generate confident matches
      MATCH_TYPE = dplyr::case_when(
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

  # Final Results
  match <- match %>%
    dplyr::union_all(exact)

  # Determine non-matches
  non_matches <- df_one %>%
    dplyr::anti_join(y = match %>% dplyr::select(ID_ONE)) %>%
    dplyr::mutate(
      ID_TWO = NA,
      FORENAME_TWO = NA,
      SURNAME_TWO= NA,
      DOB_TWO = NA,
      POSTCODE_TWO = NA,
      JW_SURNAME = NA,
      JW_FORENAME = NA,
      JW_POSTCODE = NA,
      ED_DOB = NA,
      MATCH_TYPE = "No Match"
    )

  # Add non-matches
  all_match <- match %>%
    dplyr::union_all(non_matches) %>%
    # Calculate match_count per primary dfID
    dplyr::group_by(ID_ONE) %>%
    dplyr::mutate(MATCH_COUNT = dplyr::n_distinct(ID_TWO)) %>%
    dplyr::ungroup() %>%
    # Rename back to OG column names
    dplyr::rename(
      {{ id_one }} := ID_ONE,
      {{ forename_one }} := FORENAME_ONE,
      {{ surname_one }} := SURNAME_ONE,
      {{ dob_one }} := DOB_ONE,
      {{ postcode_one }} := POSTCODE_ONE,
      {{ id_two }} := ID_TWO,
      {{ forename_two }} := FORENAME_TWO,
      {{ surname_two }} := SURNAME_ONE,
      {{ dob_two }} := DOB_TWO,
      {{ postcode_two }} := POSTCODE_TWO
    )

  # Return data
  return(all_match)
}

#' Function to find all matches form the db
#'
#' Find the exact and confident matches of a lookup df to a primary df
#'
#' @param df_one The primary df
#' @param df_two The lookup df (being matched against)
#' @param id_one primary df unique identifier
#' @param forename_one primary df forename
#' @param surname_one primary df surname
#' @param dob_one primary df DOB
#' @param postcode_one primary df postcode
#' @param id_two lookup df unique identifier
#' @param forename_two lookup df forename
#' @param surname_two lookup df surname
#' @param dob_two lookup df DOB
#' @param postcode_two lookup df postcode
#'
#' @return The primary df matched against the lookup df
#' @return This will a combinaation of exact, confident and non-matches
#'
#' @export
#'
#' @examples
#' find_db_matches(
#' df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
#' df_two, id_two, forename_two, surname_two, dob_two, postcode_two)
find_db_matches_three <- function(
  df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
  df_two, id_two, forename_two, surname_two, dob_two, postcode_two
){

  # For convenience rename columns
  df_one <- df_one %>%
    rename(
      ID_ONE := {{ id_one }},
      FORENAME_ONE := {{ forename_one }},
      SURNAME_ONE := {{ surname_one }},
      DOB_ONE := {{ dob_one }},
      POSTCODE_ONE := {{ postcode_one }}
    )

  # For convenience rename columns
  df_two <- df_two %>%
    rename(
      ID_TWO := {{ id_two }},
      FORENAME_TWO := {{ forename_two }},
      SURNAME_TWO := {{ surname_two }},
      DOB_TWO := {{ dob_two }},
      POSTCODE_TWO := {{ postcode_two }}
    )

  # Exact matches
  exact <- df_one %>%
    dplyr::select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE) %>%
    dplyr::inner_join(
      y = df_two %>%
        dplyr::select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO),
      by = c(
        "FORENAME_ONE" = "FORENAME_TWO",
        "SURNAME_ONE" = "SURNAME_TWO",
        "DOB_ONE" = "DOB_TWO",
        "POSTCODE_ONE" = "POSTCODE_TWO"
      )
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      FORENAME_TWO = FORENAME_ONE,
      SURNAME_TWO = SURNAME_ONE,
      DOB_TWO = DOB_ONE,
      POSTCODE_TWO = POSTCODE_ONE,
      JW_FORENAME = 1,
      JW_SURNAME = 1,
      JW_POSTCODE = 1,
      ED_DOB = 0,
      MATCH_TYPE = 'Exact',
    ) %>%
    dplyr::select(ID_ONE, ID_TWO, everything())

  # Remaining records
  remain <- df_one %>%
    dplyr::anti_join(y = exact, by = "ID_ONE")

  # List of permutation-join columns
  perm_num <- paste0("PERM", 1:9)

  # Distinct list of ID perm-join pairs
  id_pairs <- perm_num %>%
    purrr::map(~{

      remain %>%
        dplyr::select(ID_ONE, {{.x}}) %>%
        dplyr::inner_join(
          df_two %>%
            dplyr::select(ID_TWO, {{.x}})
        ) %>%
        dplyr::select(ID_ONE, ID_TWO)
    }) %>%
    purrr::reduce(function(x, y) union(x, y)) %>%
    dplyr::distinct()

  # Remove Permutation info from matching dfs
  df_one <- df_one %>%
    dplyr::select(ID_ONE, FORENAME_ONE, SURNAME_ONE, DOB_ONE, POSTCODE_ONE)

  # Remove Permutation info from matching dfs
  df_two <- df_two %>%
    dplyr::select(ID_TWO, FORENAME_TWO, SURNAME_TWO, DOB_TWO, POSTCODE_TWO)

  # Join lookup back to original dfs
  id_pairs <- id_pairs %>%
    dplyr::inner_join(y = df_one, by = "ID_ONE") %>%
    dplyr::inner_join(y = df_two, by = "ID_TWO")

  # Generate list of feasible dob-pairs with 6 identical characters
  eligible_dates <- dob_lv_filter(
    df = id_pairs,
    dob_one = DOB_ONE,
    dob_two = DOB_TWO
  )

  # Filter permutation output by feasible date-pair list
  cross <- id_pairs %>%
    dplyr::inner_join(eligible_dates)

  # Generate a list of feasible forename pairs
  eligible_names <- calc_db_jw_threshold(
    df = cross,
    name_one = FORENAME_ONE,
    name_two = FORENAME_TWO,
    threshold_val = 0.75,
    col_name = 'JW_FORENAME'
  )

  # Filter permutation output by feasible date-pair list
  match <- cross %>%
    dplyr::inner_join(eligible_names) %>%
    dplyr::mutate(
      # NAs for zeros
      JW_SURNAME = UTL_MATCH.JARO_WINKLER(SURNAME_ONE, SURNAME_TWO),
      JW_POSTCODE = UTL_MATCH.JARO_WINKLER(POSTCODE_ONE, POSTCODE_TWO),
      ED_DOB = ifelse(DOB_ONE == DOB_TWO, 0, 2),
      # Generate confident matches
      MATCH_TYPE = dplyr::case_when(
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

  # Final Results
  match <- match %>%
    dplyr::union_all(exact)

  # Determine non-matches
  non_matches <- df_one %>%
    dplyr::anti_join(y = match %>% dplyr::select(ID_ONE)) %>%
    dplyr::mutate(
      ID_TWO = NA,
      FORENAME_TWO = NA,
      SURNAME_TWO= NA,
      DOB_TWO = NA,
      POSTCODE_TWO = NA,
      JW_SURNAME = NA,
      JW_FORENAME = NA,
      JW_POSTCODE = NA,
      ED_DOB = NA,
      MATCH_TYPE = "No Match"
    )

  # Add non-matches
  all_match <- match %>%
    dplyr::union_all(non_matches) %>%
    # Calculate match_count per primary dfID
    dplyr::group_by(ID_ONE) %>%
    dplyr::mutate(MATCH_COUNT = dplyr::n_distinct(ID_TWO)) %>%
    dplyr::ungroup() %>%
    # Rename back to OG column names
    dplyr::rename(
      {{ id_one }} := ID_ONE,
      {{ forename_one }} := FORENAME_ONE,
      {{ surname_one }} := SURNAME_ONE,
      {{ dob_one }} := DOB_ONE,
      {{ postcode_one }} := POSTCODE_ONE,
      {{ id_two }} := ID_TWO,
      {{ forename_two }} := FORENAME_TWO,
      {{ surname_two }} := SURNAME_ONE,
      {{ dob_two }} := DOB_TWO,
      {{ postcode_two }} := POSTCODE_TWO
    )

  # Return data
  return(all_match)
}

