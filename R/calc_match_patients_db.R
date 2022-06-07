#' Find all potential person matches between two datasets based on personal identifiable information
#'
#' The two input datasets should be 'lazyframes', generated from a connnection to a database.
#' The first dataset (df_one) containing the people to look for and the second dataset (df2)
#' containing the data to be searched against. Parameters can be used to specify the formatting
#' of the output dataset and allow some customisation of the match scoring.
#'
#' @param df_one dataframe containing person level information representing people to look for
#' @param id_one unique reference column for df1
#' @param forename_one forename column for df1
#' @param surname_one surname column for df1
#' @param dob_one date of birth column for df1, ideally in dd/mm/yyyy format although others are
#' handled
#' @param postcode_one postcode field for df1
#' @param df_one dataframe containing person level information representing people to look in for
#' matches
#' @param id_two unique reference column for df2
#' @param forename_two forename column for df2
#' @param surname_two surname column for df2
#' @param dob_two date of birth column for df2, ideally in dd/mm/yyyy format although others are
#' handled
#' @param postcode_two postcode field for df2
#' @param output_type One of the following: "key" / "match" / "all"
#' @param format_data TRUE/FALSE : identifying if the input datasets should be passed through
#' formatting functions to clean data prior to matching
#' @param inc_no_match TRUE/FALSE : identifying if the output should include non matches
#' @param sw_forename (default = 0.30) proportion weighting value (0.0-1.0) to be applied to the forename part of the match score
#' @param sw_surname (default = 0.15) proportion weighting value (0.0-1.0) to be applied to the surname part of the match score
#' @param sw_dob (default = 0.40) proportion weighting value (0.0-1.0) to be applied to the dob part of the match score
#' @param sw_postcode (default = 0.15) proportion weighting value (0.0-1.0) to be applied to the postcode part of the match score
#'
#' @return A 'lazyframe' comprising of all potential matches between two datasets, to either be collected or written back to a database.
#' @export
#'
#' @examples
#' calc_match_patients_db(df_one, id_one, ...)
calc_match_patients_db <- function(df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
                                   df_two, id_two, forename_two, surname_two, dob_two, postcode_two,
                                   output_type = c("all", "key", "match"),
                                   format_data = c(TRUE, FALSE),
                                   inc_no_match = c(TRUE, FALSE),
                                   sw_forename = 0.3, sw_surname = 0.15, sw_dob = 0.4, sw_postcode = 0.15) {

  # Match arguments
  match.arg(output_type)

  # check if the match score weightings add up to 100%
  # Check if any of the parameters have been entered as a non numeric value
  if (!is.numeric(sw_forename) ||
    !is.numeric(sw_surname) ||
    !is.numeric(sw_dob) ||
    !is.numeric(sw_postcode)) {
    # non numeric value supplied as weighting factor
    stop("Non numeric value supplied as weighting factor", call. = FALSE)
  } else if (!dplyr::between(sw_forename, 0, 1) ||
    !dplyr::between(sw_surname, 0, 1) ||
    !dplyr::between(sw_dob, 0, 1) ||
    !dplyr::between(sw_postcode, 0, 1)) {
    # invalid values applied
    stop("Individual field score weighting values must be between 0.0 and 1.0", call. = FALSE)
  } else if ((sw_forename + sw_surname + sw_dob + sw_postcode) != 1) {
    # if the proportions do not equal 100% abort the process and show an error message
    stop("Supplied score weighting values do not total 100%", call. = FALSE)
  }

  # Check if matching db tables have matching column names
  if (max(colnames(df_one) %in% colnames(df_two)) == 1) {
    stop("Each dataset requires unique column names.
         Matched database tables cannot contain duplicate column names.
         Rename columns then use the function again.", call. = FALSE)
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
  if (format_data == TRUE) {

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
      na_matches = "never"
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
      na_matches = "never"
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
      DOB_SCORE = 1,
      MATCH_TYPE = "Exact",
      MATCH_SCORE = 1
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
    purrr::map(~ {
      remain %>%
        dplyr::select(all_of(df_one_cols), {{ .x }}) %>%
        dplyr::inner_join(
          df_two %>%
            dplyr::select(all_of(df_two_cols), {{ .x }}),
          by = {{ .x }}
        ) %>%
        dplyr::select(-{{ .x }})
    }) %>%
    purrr::reduce(function(x, y) dplyr::union(x, y)) %>%
    dplyr::distinct()

  # Generate list of feasible dob-pairs with 6 identical characters
  cross <- id_pairs %>%
    filter_name_db(., FORENAME_ONE, FORENAME_TWO) %>%
    filter_dob_db(., DOB_ONE, DOB_TWO, 0.75)

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
        (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DOB_SCORE == 1) ~ "Exact",
        (JW_SURNAME == 1 & JW_FORENAME == 1 & DOB_SCORE == 1) ~ "Confident",
        (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DOB_SCORE >= 0.75) ~ "Confident",
        (JW_FORENAME == 1 & JW_POSTCODE == 1 & DOB_SCORE == 1) ~ "Confident",
        (JW_SURNAME == 1 & JW_FORENAME >= 0.75 & JW_POSTCODE == 1 & DOB_SCORE == 1) ~ "Confident",
        (JW_SURNAME >= 0.9 & JW_FORENAME >= 0.9 & JW_POSTCODE >= 0.9 & DOB_SCORE == 1) ~ "Confident",
        TRUE ~ "No Match"
      )
    ) %>%
    # filter to only confident matches
    dplyr::filter(MATCH_TYPE != "No Match") %>%
    # calculate an overall weighted score
    dplyr::mutate(MATCH_SCORE = (
      (ifelse(is.na(JW_FORENAME), 0, JW_FORENAME) * sw_forename) +
        (ifelse(is.na(JW_SURNAME), 0, JW_SURNAME) * sw_surname) +
        (ifelse(is.na(DOB_SCORE), 0, DOB_SCORE) * sw_dob) +
        (ifelse(is.na(JW_POSTCODE), 0, JW_POSTCODE) * sw_postcode)
    )) %>%
    # Add exact matches
    dplyr::union_all(exact_matches) %>%
    # Calculate match_count per primary df ID
    dplyr::group_by(ID_ONE) %>%
    dplyr::mutate(MATCH_COUNT = dplyr::n_distinct(ID_TWO)) %>%
    dplyr::ungroup()

  # Whether or not to return non-matches
  if (inc_no_match == TRUE) {

    # Determine missing non-match fields
    non_matches <- df_one %>%
      dplyr::anti_join(y = matches %>% dplyr::select(ID_ONE)) %>%
      dplyr::mutate(
        ID_TWO = NA,
        FORENAME_TWO = NA,
        SURNAME_TWO = NA,
        DOB_TWO = NA,
        POSTCODE_TWO = NA,
        JW_SURNAME = NA,
        JW_FORENAME = NA,
        JW_POSTCODE = NA,
        DOB_SCORE = NA,
        MATCH_TYPE = "No Match",
        MATCH_SCORE = 0,
        MATCH_COUNT = 0
      )

    # Add non-matches to all-matches
    all_matches <- matches %>%
      dplyr::union_all(non_matches)
  } else {

    # All matches doesn't include non-matches
    all_matches <- matches
  }

  # Determine final output format
  if (output_type == "key") {

    # Only select key columns
    all_matches <- all_matches %>%
      dplyr::select(
        {{ id_one }} := ID_ONE,
        {{ id_two }} := ID_TWO,
        MATCH_TYPE,
        MATCH_COUNT,
        MATCH_SCORE
      )
  } else if (output_type == "match") {

    # Only select key columns
    all_matches <- all_matches %>%
      dplyr::select(
        {{ id_one }} := ID_ONE,
        {{ forename_one }} := FORENAME_ONE,
        {{ surname_one }} := SURNAME_ONE,
        {{ dob_one }} := DOB_ONE,
        {{ postcode_one }} := POSTCODE_ONE,
        {{ id_two }} := ID_TWO,
        {{ forename_two }} := FORENAME_TWO,
        {{ surname_two }} := SURNAME_TWO,
        {{ dob_two }} := DOB_TWO,
        {{ postcode_two }} := POSTCODE_TWO,
        MATCH_TYPE,
        MATCH_COUNT,
        MATCH_SCORE
      )
  } else if (output_type == "all") {

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
        MATCH_SCORE,
        FORENAME_SCORE = JW_FORENAME,
        SURNAME_SCORE = JW_SURNAME,
        DOB_SCORE,
        POSTCODE_SCORE = JW_POSTCODE
      )

    # join back to df1 and df2 to get any missing columns
    # remove any of the key columns from df1 and df2 to stop duplicates
    # prefix the additional columns with an identifier for the source dataset
    all_matches <- all_matches %>%
      dplyr::left_join(
        df_one %>%
          dplyr::select(-FORENAME_ONE, -SURNAME_ONE, -DOB_ONE, -POSTCODE_ONE) %>%
          dplyr::rename_all(list(~ paste0("DF1_", .))),
        by = c("ID_ONE" = "DF1_ID_ONE")
      ) %>%
      dplyr::left_join(
        df_two %>%
          dplyr::select(
            -FORENAME_TWO, -SURNAME_TWO, -DOB_TWO, -POSTCODE_TWO,
            -PERM1, -PERM2, -PERM3, -PERM4, -PERM5, -PERM6, -PERM7, -PERM8, -PERM9
          ) %>%
          dplyr::rename_all(list(~ paste0("DF2_", .))),
        by = c("ID_TWO" = "DF2_ID_TWO")
      )

    # rename to match with input
    all_matches <- all_matches %>%
      dplyr::select(
        {{ id_one }} := ID_ONE,
        {{ forename_one }} := FORENAME_ONE,
        {{ surname_one }} := SURNAME_ONE,
        {{ dob_one }} := DOB_ONE,
        {{ postcode_one }} := POSTCODE_ONE,
        {{ id_two }} := ID_TWO,
        {{ forename_two }} := FORENAME_TWO,
        {{ surname_two }} := SURNAME_TWO,
        {{ dob_two }} := DOB_TWO,
        {{ postcode_two }} := POSTCODE_TWO,
        MATCH_TYPE,
        MATCH_COUNT,
        MATCH_SCORE,
        dplyr::everything()
      )
  }

  # Return data
  return(all_matches)
}
