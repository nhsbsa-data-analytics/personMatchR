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
    select({{ dob_one }}, {{ dob_two }}) %>%
    distinct()

  # Function to apply multiple times
  dob_substr <- function(df, sub_one, sub_two, sub_three){

    df %>%
      filter(
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
    mutate(
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
      # First 3 chars forename, plus 3 chars of surname and postcode
      PERM5 = paste0(
        substr({{ forename }}, 1, 3),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # Last 3 chars forename, plus 3 chars of surname and postcode
      PERM6 = paste0(
        SUBSTR({{ forename }}, nchar({{ forename }})-2, 3),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # First 3 consonants, plus 3 chars of surname and postcode
      PERM7 = paste0(
        substr(REGEXP_REPLACE({{ forename}}, '[AEIOU]', ''), 1, 3),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # All consonants, plus 4 chars of surname and postcode
      PERM8 = paste0(
        REGEXP_REPLACE({{ forename}}, '[AEIOU]', ''),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # All vowels, plus 3 chars of surname and postcode
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
#'
calc_db_jw_threshold <- function(df, name_one, name_two, threshold_val, col_name){

  # Distinct df to calculate JW scores for
  output <- df %>%
    rename(
      NAME_ONE := {{ name_one }},
      NAME_TWO := {{ name_two }}
    ) %>%
    distinct() %>%
    name_db_filter(., NAME_ONE, NAME_TWO) %>%
    mutate(ID = row_number(NAME_ONE))

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

  output <- inner_join(
    x = dplyr::tbl(db_connection, dplyr::sql(sql_query_one)),
    y = dplyr::tbl(db_connection, dplyr::sql(sql_query_two))
    )

  output <- output %>%
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
    # Filter by threshold value
    filter(JW_APPROX >= threshold_val) %>%
    select(NAME_ONE, NAME_TWO, JW_APPROX) %>%
    distinct() %>%
    mutate(JW_APPROX = UTL_MATCH.JARO_WINKLER(NAME_ONE, NAME_TWO)) %>%
    filter(JW_APPROX >= threshold_val) %>%
    rename_at("JW_APPROX", ~col_name) %>%
    rename(
      {{ name_one }} := NAME_ONE,
      {{ name_two }} := NAME_TWO
    )

  # Return output
  return(output)
}
