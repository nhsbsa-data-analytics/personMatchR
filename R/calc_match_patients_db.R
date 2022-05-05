#' Function to find all matches from the db
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
#' @param output_type which group of fields the user wants returning
#' @param format_data whether or not to format the data (which adds runtime)
#'
#' @return The primary df matched against the lookup df
#' @return This will a combinaation of exact, confident and non-matches
#'
#' @export
#'
#' @examples
#' calc_match_patients_db(
#' df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
#' df_two, id_two, forename_two, surname_two, dob_two, postcode_two
#' output_type, format_data)
calc_match_patients_db <- function(
  df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
  df_two, id_two, forename_two, surname_two, dob_two, postcode_two,
  output_type = c("all", "key", "match"),
  format_data = c(TRUE, FALSE)
){

  # Match arguments
  match.arg(output_type)

  # Return id db tables have matching column names
  if(max(colnames(df_one) %in% colnames(df_one)) == 1){

    print("ERROR: Each dataset requires unique column names.")
    print("REASON: Matched database tables cannot contain duplicate column names")
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
      format_postcode_db(., POSTCODE_ONE) %>%
      format_name_db(., FORENAME_ONE) %>%
      format_name_db(., SURNAME_ONE) %>%
      format_date_db(., DOB_ONE)

    # Format df two
    df_two <- df_two %>%
      format_postcode_db(., POSTCODE_TWO) %>%
      format_name_db(., FORENAME_TWO) %>%
      format_name_db(., SURNAME_TWO) %>%
      format_date_db(., DOB_TWO)
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
      ),
      na_match = "never"
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
      ),
      na_match = "never"
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
      DIFF_DOB = 0,
      MATCH_TYPE = 'Exact',
    )

  # Remaining records
  remain <- df_one %>%
    dplyr::anti_join(y = exact_matches, by = "ID_ONE") %>%
    calc_permutations_db(., FORENAME_ONE, SURNAME_ONE, POSTCODE_ONE, DOB_ONE)

  # Select columns and calculate permutations
  df_two <- df_two %>%
    calc_permutations_db(., FORENAME_TWO, SURNAME_TWO, POSTCODE_TWO, DOB_TWO)

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
    filter_name_db(., FORENAME_ONE, FORENAME_TWO) %>%
    filter_dob_db(., DOB_ONE, DOB_TWO, 2)

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
      DIFF_DOB = NA,
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
