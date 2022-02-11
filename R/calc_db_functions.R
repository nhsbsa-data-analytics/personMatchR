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
dob_lv_filter <- function(df, dob_col_one, dob_col_two){

  # Rename df for convenience
  df <- df %>%
    rename(
      DOB_ONE := {{ dob_col_one }},
      DOB_TWO := {{ dob_col_two }}
    )

  # Function to apply multiple times
  dob_substr <- function(df, sub_one, sub_two, sub_three){

    df %>%
      filter(
        SUBSTR(DOB_ONE, sub_one, 1) == SUBSTR(DOB_TWO, sub_one, 1) |
          # Tokens share same second letter
          SUBSTR(DOB_ONE, sub_two, 1) == SUBSTR(DOB_TWO, sub_two, 1) |
          # Tokens share same second letter
          SUBSTR(DOB_ONE, sub_three, 1) == SUBSTR(DOB_TWO, sub_three, 1),
        DOB_ONE != DOB_TWO
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

  # Original df names
  df <- df %>%
    rename(
      {{ dob_col_one }} := DOB_ONE,
      {{ dob_col_two }} := DOB_TWO
      )

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
        INSTR({{ name_two  }}, {{ name_one  }}) > 1,
      # Deal with single-letter 'names' separately
      nchar({{ name_one  }}) != 1,
      nchar({{ name_two  }}) != 1
    )
}

#' Calculates 5 permutations for primary-lookup join prior to match scoring
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
    mutate(
      PERM1 = paste0({{ forename }}, {{ surname }}, {{ dob }}),
      PERM2 = paste0({{ forename }}, {{ postcode }}, {{dob}}),
      PERM3 = paste0({{ surname }}, {{ postcode }}, {{ dob }}),
      PERM4 = paste0(substr({{ forename }}, 1, 1), substr({{ surname }}, 1, 2), substr({{ postcode }}, 1, 2)),
      PERM5 = paste0(SUBSTR({{ forename }}, nchar({{ forename }})-2, 3), substr({{ surname }}, 1, 2), substr({{ postcode }}, 1, 2))
    )
}

#' Calculates 2 additional permutations for primary-lookup join prior to scoring
#'
#' @param df A df to be formatted
#' @param forename forename column name
#' @param surname surname column name
#' @param postcode postcode column name
#' @param dob DOB column name
#'
#' @return A df with 2 'alphabetical' join permutations added
#'
#' @export
#' calc_alpha_permutations(df, id, forename, surname, postcode)
calc_alpha_permutations <- function(df, id, forename, surname, postcode){

  output <- df %>%
    select({{ id }}, {{ forename }}, {{ surname }}, {{ postcode }}) %>%
    # Rename for convenience and rename back later
    rename(
      ID := {{ id }},
      FORENAME := {{ forename }},
      SURNAME := {{ surname }},
      POSTCODE := {{ postcode }}
    ) %>%
    # Tokenise by character
    mutate(TOKEN_ONE = trimws(REGEXP_REPLACE(FORENAME, '*', ' '))) %>%
    nhsbsaR::oracle_unnest_tokens(col = 'TOKEN_ONE') %>%
    group_by(ID) %>%
    # Create forward and reverse alphabetical character order
    mutate(
      ALPHA = row_number(TOKEN),
      ALPHA_B = row_number(desc(TOKEN))
    ) %>%
    ungroup()

  # First 3 alphabetical characters
  one <- output %>%
    filter(ALPHA <= 3) %>%
    select(ID, SURNAME, POSTCODE, TOKEN, ALPHA)

  # Last 3 alphabetical characters
  two <- output %>%
    filter(ALPHA_B <= 3) %>%
    select(ID, SURNAME, POSTCODE, TOKEN, ALPHA = ALPHA_B)

  # Pull the DB connections (2)
  db_connection_one <- one$src$con
  db_connection_two <- two$src$con

  # Build SQL Query: one
  sql_query_one <- dbplyr::build_sql(
    con = db_connection_one,
    "
    SELECT ID, SURNAME, POSTCODE,
    LISTAGG(TOKEN, '') within group (order by ALPHA) as ALPHA_FORWARD
    FROM (", dbplyr::sql_render(one), ")
    GROUP BY ID, SURNAME, POSTCODE"
  )

  # Build SQL Query: two
  sql_query_two <- dbplyr::build_sql(
    con = db_connection_two,
    "
    SELECT ID, SURNAME, POSTCODE,
    LISTAGG(TOKEN, '') within group (order by ALPHA) as ALPHA_BACK
    FROM (", dbplyr::sql_render(two), ")
    GROUP BY ID, SURNAME, POSTCODE"
  )

  # Query Outputs (2)
  output_one <- dplyr::tbl(src = db_connection, dplyr::sql(sql_query_one))
  output_two <- dplyr::tbl(src = db_connection, dplyr::sql(sql_query_two))

  # Permutation Six
  output_one <- output_one %>%
    mutate(PERM8 = paste0(ALPHA_FORWARD, substr(SURNAME, 1, 2), substr(POSTCODE, 1, 2))) %>%
    select(ID, PERM8) %>%
    rename({{ id }} := ID)

  # Permutation Seven
  output_two <- output_two %>%
    mutate(PERM7 = paste0(ALPHA_BACK, substr(SURNAME, 1, 2), substr(POSTCODE, 1, 2))) %>%
    select(ID, PERM7) %>%
    rename({{ id }} := ID)

  # Rejoin back to original df then return
  df <- df %>%
    left_join(output_one) %>%
    left_join(output_two)

  # Return DF
  return(df)
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
#'
#' @return A df only with name-pairs with a JW value above a threshold
#'
#' @export
#'
#' @examples
#' calc_db_jw_threshold(df, name_one, name_two, threshold_val)
calc_db_jw_threshold <- function(df, name_one, name_two, threshold_val){

  # Rename inputs for convenience & generate ID
  df <- df %>%
    rename(
      NAME_ONE = {{ name_one }},
      NAME_TWO = {{ name_two }}
    ) %>%
    mutate(ID = row_number(NAME_ONE))

  # Split name by character & count token-instances per name (e.d. A1, A2 etc)
  one <- df %>%
    mutate(TOKEN_ONE = trimws(REGEXP_REPLACE(NAME_ONE, '*', ' '))) %>%
    nhsbsaR::oracle_unnest_tokens(col = 'TOKEN_ONE') %>%
    group_by(ID, TOKEN) %>%
    mutate(TOKEN = paste0(TOKEN, rank(TOKEN_NUMBER))) %>%
    ungroup() %>%
    select(ID, NAME_ONE, NUMBER_ONE = TOKEN_NUMBER, TOKEN)

  # Split name by character & count token-instances per name (e.d. A1, A2 etc)
  two <- df %>%
    mutate(TOKEN_TWO = trimws(REGEXP_REPLACE(NAME_TWO, '*', ' '))) %>%
    nhsbsaR::oracle_unnest_tokens(col = 'TOKEN_TWO') %>%
    group_by(ID, TOKEN) %>%
    mutate(TOKEN = paste0(TOKEN, rank(TOKEN_NUMBER))) %>%
    ungroup() %>%
    select(ID, NAME_TWO, NUMBER_TWO = TOKEN_NUMBER, TOKEN)

  # Join on ID & token (A1, A2 etc) then calculate matching chars
  jw <- one %>%
    inner_join(two, by = c("ID", "TOKEN")) %>%
    group_by(ID) %>%
    mutate(
      # NOTE: char-distance is ignored, as only approx calculation
      M = n(),
      # Length of each name required for JW approx
      S_ONE = nchar(NAME_ONE),
      S_TWO = nchar(NAME_TWO),
      # Number of transpositions (T) = 0, again to speed up approx calculation
      SIM = (1/3) * (((M / S_ONE) + (M / S_TWO) + ((M - 0) / M))),
      # Final JW value, number of shared first four letters
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
    distinct() %>%
    # Filter by threshold value
    filter(JW_APPROX >= threshold_val) %>%
    # Now calculate 'real' JW on reduced list of names
    mutate(JW = UTL_MATCH.JARO_WINKLER(NAME_ONE, NAME_TWO)) %>%
    # Filter by threshold value
    filter(JW >= threshold_val) %>%
    select(NAME_ONE, NAME_TWO, JW) %>%
    # Revert cols to original names
    rename(
      {{ name_one }} := NAME_ONE,
      {{ name_two }} := NAME_TWO
    )

  # Return output
  return(jw)
}

#' More efficient distinct with large number of records
#'
#' Generate
#'
#' @param df A df to be formatted
#'
#' @return A df distinct by all columns
#'
#' @export
#'
#' @examples
#' calc_db_jw_threshold(df, name_one, name_two, threshold_val)
group_by_distinct <- function(df){

  df <- df %>%
    group_by_all() %>%
    summarise() %>%
    ungroup()
}
