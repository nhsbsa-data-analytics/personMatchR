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
#' dob_substr(df, dob_one, dob_two, dob_diff
dob_db_filter <- function(df, dob_one, dob_two, diff_threshold){

  df %>%
    dplyr::mutate(
      # Generate 8 character-level matching binary scores
      CHAR1 = ifelse(substr({{ dob_one }},1,1) == substr({{ dob_two }},1,1), 1, 0),
      CHAR2 = ifelse(substr({{ dob_one }},2,2) == substr({{ dob_two }},2,2), 1, 0),
      CHAR3 = ifelse(substr({{ dob_one }},3,3) == substr({{ dob_two }},3,3), 1, 0),
      CHAR4 = ifelse(substr({{ dob_one }},4,4) == substr({{ dob_two }},4,4), 1, 0),
      CHAR5 = ifelse(substr({{ dob_one }},5,5) == substr({{ dob_two }},5,5), 1, 0),
      CHAR6 = ifelse(substr({{ dob_one }},6,6) == substr({{ dob_two }},6,6), 1, 0),
      CHAR7 = ifelse(substr({{ dob_one }},7,7) == substr({{ dob_two }},7,7), 1, 0),
      CHAR8 = ifelse(substr({{ dob_one }},8,8) == substr({{ dob_two }},8,8), 1, 0),
      # Total scores and present as DOB difference
      DIFF_DOB = 8 - (CHAR1 + CHAR2 + CHAR3 + CHAR4 + CHAR5 + CHAR6 + CHAR7 + CHAR8)
    ) %>%
    dplyr::filter(DIFF_DOB <= diff_threshold) %>%
    dplyr::select(-c(CHAR1, CHAR2, CHAR3, CHAR4, CHAR5, CHAR6, CHAR7, CHAR8))
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

  df %>%
    dplyr::filter(
      # Tokens share the same first letter
      SUBSTR({{ name_one }}, 1, 1) == SUBSTR({{ name_two }}, 1, 1) |
        # Tokens share same second letter
        SUBSTR({{ name_one }}, 2, 1) == SUBSTR({{ name_two }}, 2, 1) |
        # Tokens share same last letter
        SUBSTR({{ name_one  }}, LENGTH({{ name_one }}), 1) == SUBSTR({{ name_two }}, LENGTH({{ name_two }}), 1) |
        # One token is a substring of the other
        INSTR({{ name_one }}, {{ name_two }}) > 1 |
        INSTR({{ name_two }}, {{ name_one }}) > 1
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
find_db_matches <- function(
  df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
  df_two, id_two, forename_two, surname_two, dob_two, postcode_two,
  output_type = c("all", "key", "match"),
  format_data = c(TRUE, FALSE)
){

  # Match arguments
  match.arg(output_type)

  # Check all column names unique
  df_one_cols_original <- colnames(df_one)
  df_two_cols_original <- colnames(df_two)

  if(max(colnames(pds_db) %in% colnames(eib_db)) == 1){

    print("ERROR: Each dataset requires unique column names.")
    print("EDIT: Rename columns then use the function again.")
    return()
  }

  # Rename columns
  df_one <- df_one %>%
    rename(
      ID_ONE := {{ id_one }},
      FORENAME_ONE := {{ forename_one }},
      SURNAME_ONE := {{ surname_one }},
      DOB_ONE := {{ dob_one }},
      POSTCODE_ONE := {{ postcode_one }}
    )

  # Rename columns
  df_two <- df_two %>%
    rename(
      ID_TWO := {{ id_two }},
      FORENAME_TWO := {{ forename_two }},
      SURNAME_TWO := {{ surname_two }},
      DOB_TWO := {{ dob_two }},
      POSTCODE_TWO := {{ postcode_two }}
    )

  # Format data depending on function input selection
  if(format_data == TRUE){

    # Format df one
    df_one <- df_one %>%
      format_db_postcode(., POSTCODE_ONE) %>%
      format_db_name(., FORENAME_ONE) %>%
      format_db_name(., SURNAME_ONE) %>%
      format_db_date(., DOB_ONE)

    # Format df two
    df_two <- df_two %>%
      format_db_postcode(., POSTCODE_TWO) %>%
      format_db_name(., FORENAME_TWO) %>%
      format_db_name(., SURNAME_TWO) %>%
      format_db_date(., DOB_TWO)
  }

  # Df column names
  df_one_cols <- colnames(df_one)
  df_two_cols <- colnames(df_two)

  # Exact matches
  exact_matches <- df_one %>%
    dplyr::inner_join(
      y = df_two,
      by = c(
        "FORENAME_ONE" = "FORENAME_TWO",
        "SURNAME_ONE" = "SURNAME_TWO",
        "DOB_ONE" = "DOB_TWO",
        "POSTCODE_ONE" = "POSTCODE_TWO"
      )
    )

  # Reverse exact matches
  exact_matches_reverse <- df_one %>%
    dplyr::inner_join(
      y = df_two,
      by = c(
        "FORENAME_ONE" = "SURNAME_TWO",
        "SURNAME_ONE" = "FORENAME_TWO",
        "DOB_ONE" = "DOB_TWO",
        "POSTCODE_ONE" = "POSTCODE_TWO"
      )
    )

  # Union exact matches
  exact_matches <- exact_matches %>%
    dplyr::union_all(exact_matches_reverse) %>%
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
    )

  # Remaining records
  remain <- df_one %>%
    dplyr::anti_join(y = exact_matches, by = "ID_ONE") %>%
    calc_permutations(., FORENAME_ONE, SURNAME_ONE, POSTCODE_ONE, DOB_ONE)

  # Select columns and calculate permutations
  df_two <- df_two %>%
    calc_permutations(., FORENAME_TWO, SURNAME_TWO, POSTCODE_TWO, DOB_TWO)

  # List of permutation-join columns
  perm_num <- paste0("PERM", 1:9)

  # Distinct list of ID perm-join pairs
  id_pairs <- perm_num %>%
    purrr::map(~{

      remain %>%
        dplyr::select(all_of(df_one_cols), {{.x}}) %>%
        dplyr::inner_join(
          df_two %>%
            dplyr::select(all_of(df_two_cols), {{.x}}),
          by = {{.x}}
        ) %>%
        dplyr::select(- {{.x}})
    }) %>%
    purrr::reduce(function(x, y) union(x, y)) %>%
    dplyr::distinct()

  # Generate list of feasible dob-pairs with 6 identical characters
  cross <- id_pairs %>%
    name_db_filter(., FORENAME_ONE, FORENAME_TWO) %>%
    dob_db_filter(., DOB_ONE, DOB_TWO, 2)

  # Generate a list
  matches <- cross %>%
    dplyr::mutate(
      # If single character forename handle differently, otherwise JW
      JW_FORENAME = dplyr::case_when(
        length(FORENAME_ONE) == 1 & FORENAME_ONE == substr(FORENAME_TWO, 1, 1) ~ 0.75,
        FORENAME_ONE == FORENAME_TWO ~ 1,
        T ~ UTL_MATCH.JARO_WINKLER(FORENAME_ONE, FORENAME_TWO)
      )
    ) %>%
    dplyr::filter(JW_FORENAME >= 0.75) %>%
    dplyr::mutate(
      # JW match, bypassing exact string matches (DIFF_DOB already calculated)
      JW_SURNAME = ifelse(SURNAME_ONE == SURNAME_TWO, 1, UTL_MATCH.JARO_WINKLER(SURNAME_ONE, SURNAME_TWO)),
      JW_POSTCODE = ifelse(POSTCODE_ONE == POSTCODE_TWO, 1, UTL_MATCH.JARO_WINKLER(POSTCODE_ONE, POSTCODE_TWO)),
      # Generate confident matches
      MATCH_TYPE = dplyr::case_when(
        (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DIFF_DOB == 0) ~ "Exact",
        (JW_SURNAME == 1 & JW_FORENAME == 1 & DIFF_DOB == 0) ~ "Confident",
        (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DIFF_DOB <= 2) ~ "Confident",
        (JW_FORENAME == 1 & JW_POSTCODE == 1 & DIFF_DOB == 0) ~ "Confident",
        (JW_SURNAME == 1 & JW_FORENAME >= 0.75 & JW_POSTCODE == 1 & DIFF_DOB == 0) ~ "Confident",
        (JW_SURNAME >= 0.85 & JW_FORENAME >= 0.75 & JW_POSTCODE >= 0.85 & DIFF_DOB <= 2) ~ "Confident",
        TRUE ~ "No Match"
      )
    ) %>%
    # filter to only confident matches
    dplyr::filter(MATCH_TYPE != "No Match") %>%
    # Add exact matches
    dplyr::union_all(exact_matches)

  # Determine missing non-match fields
  non_matches <- df_one %>%
    dplyr::anti_join(y = matches %>% dplyr::select(ID_ONE)) %>%
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
  all_matches <- matches %>%
    dplyr::union_all(non_matches) %>%
    # Calculate match_count per primary df ID
    dplyr::group_by(ID_ONE) %>%
    dplyr::mutate(MATCH_COUNT = dplyr::n_distinct(ID_TWO)) %>%
    dplyr::ungroup()

  # Determine final output format
  if(output_type == "key"){

    # Only select key columns
    all_matches <- all_matches %>%
      dplyr::select(
        ID_ONE,
        ID_TWO,
        MATCH_TYPE,
        MATCH_COUNT
        )

  }else if(output_type == "match"){

    # Only select key columns
    all_matches <- all_matches %>%
      dplyr::select(
        ID_ONE,
        FORENAME_ONE,
        SURNAME_ONE,
        DOB_ONE,
        POSTCODE_ONE,
        ID_TWO,
        FORENAME_TWO,
        SURNAME_TWO,
        DOB_TWO,
        POSTCODE_TWO,
        MATCH_TYPE,
        MATCH_COUNT
      )

  }else if(output_type == "all"){

    # Only select key columns
    all_matches <- all_matches %>%
      dplyr::select(
        ID_ONE,
        FORENAME_ONE,
        SURNAME_ONE,
        DOB_ONE,
        POSTCODE_ONE,
        ID_TWO,
        FORENAME_TWO,
        SURNAME_TWO,
        DOB_TWO,
        POSTCODE_TWO,
        MATCH_TYPE,
        MATCH_COUNT,
        dplyr::everything()
      )
  }

  # Rename back to original column names
  all_matches <- all_matches %>%
    dplyr::rename(
      {{ id_one }} := ID_ONE,
      {{ forename_one }} := FORENAME_ONE,
      {{ surname_one }} := SURNAME_ONE,
      {{ dob_one }} := DOB_ONE,
      {{ postcode_one }} := POSTCODE_ONE,
      {{ id_two }} := ID_TWO,
      {{ forename_two }} := FORENAME_TWO,
      {{ surname_two }} := SURNAME_TWO,
      {{ dob_two }} := DOB_TWO,
      {{ postcode_two }} := POSTCODE_TWO
  )

  # Return data
  return(all_matches)
}
