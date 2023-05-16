#' Find all potential person matches between two datasets based on personal identifiable information
#'
#' The two input datasets should be dataframes, with the first dataset (df_one) containing the people
#' to look for and the second dataset (df_two) containing the data to be searched against.
#' \cr\cr Parameters can be used to specify the formatting of the output dataset and allow some
#' customisation of the match scoring.
#'
#' @param df_one dataframe containing person level information representing people to look for
#' @param id_one unique reference column for df_one
#' @param forename_one forename column for df_one
#' @param surname_one surname column for df_one
#' @param dob_one date of birth column for df_one, ideally in dd/mm/yyyy format although others are
#' handled
#' @param postcode_one postcode field for df_one
#' @param df_one dataframe containing person level information representing people to look in for
#' matches
#' @param id_two unique reference column for df_two
#' @param forename_two forename column for df_two
#' @param surname_two surname column for df_two
#' @param dob_two date of birth column for df_two, ideally in dd/mm/yyyy format although others are
#' handled
#' @param postcode_two postcode field for df_two
#' @param output_type One of the following: "key" / "match" / "all"
#' @param format_data TRUE/FALSE : identifying if the input datasets should be passed through
#' formatting functions to clean data prior to matching
#' @param inc_no_match TRUE/FALSE : identifying if the output should include non matches
#' @param sw_forename (default = 0.30) proportion weighting value (0.0-1.0) to be applied to the forename part of the match score
#' @param sw_surname (default = 0.15) proportion weighting value (0.0-1.0) to be applied to the surname part of the match score
#' @param sw_dob (default = 0.40) proportion weighting value (0.0-1.0) to be applied to the dob part of the match score
#' @param sw_postcode (default = 0.15) proportion weighting value (0.0-1.0) to be applied to the postcode part of the match score
#'
#' @return dataframe comprising of all potential matches between two datasets
#' @export
#'
calc_match_person <- function(df_one, id_one, forename_one, surname_one, dob_one, postcode_one,
                                df_two, id_two, forename_two, surname_two, dob_two, postcode_two,
                                output_type = c("all", "key", "match"),
                                format_data = c(TRUE, FALSE),
                                inc_no_match = c(TRUE, FALSE),
                                sw_forename = 0.3, sw_surname = 0.15, sw_dob = 0.4, sw_postcode = 0.15) {

  # Match arguments
  match.arg(output_type)

  # check if the match score weightings add up to 100% or any of the parameters have been entered as a non numeric value
  if (!is.numeric(sw_forename) || !is.numeric(sw_surname) || !is.numeric(sw_dob) || !is.numeric(sw_postcode)) {
    # non numeric value supplied as weighting factor
    stop("Non numeric value supplied as weighting factor", call. = FALSE)
  } else if (!dplyr::between(sw_forename, 0, 1) || !dplyr::between(sw_surname, 0, 1) || !dplyr::between(sw_dob, 0, 1) || !dplyr::between(sw_postcode, 0, 1)) {
    # invalid values applied
    stop("Individual field score weighting values must be between 0.0 and 1.0", call. = FALSE)
  } else if ((sw_forename + sw_surname + sw_dob + sw_postcode) != 1) {
    # if the proportions do not equal 100% abort the process and show an error message
    stop("Supplied score weighting values do not total 100%", call. = FALSE)
  }

  # All columns names from both input dfs
  all_cols <- c(colnames(df_one), colnames(df_two))

  # List of illegible names for non function-input columns
  error_cols <- c(
    "ID_ONE", "FORENAME_ONE", "SURNAME_ONE", "DOB_ONE", "POSTCODE_ONE",
    "ID_TWO", "FORENAME_TWO", "SURNAME_TWO", "DOB_TWO", "POSTCODE_TWO"
  )

  # List of function-input column names
  input_cols <- c(
    {
      deparse(substitute(id_one))
    },
    {
      deparse(substitute(forename_one))
    },
    {
      deparse(substitute(surname_one))
    },
    {
      deparse(substitute(dob_one))
    },
    {
      deparse(substitute(postcode_one))
    },
    {
      deparse(substitute(id_two))
    },
    {
      deparse(substitute(forename_two))
    },
    {
      deparse(substitute(surname_two))
    },
    {
      deparse(substitute(dob_two))
    },
    {
      deparse(substitute(postcode_two))
    }
  )

  # List of non function-input columns
  non_input_cols <- c(setdiff(all_cols, input_cols), setdiff(input_cols, all_cols))

  # Stop if any non function-input columns have illegible names
  if (max(error_cols %in% non_input_cols) == 1) {
    stop(
      paste0(
        "Non function-input columns cannot have any of the following names: ",
        paste(error_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Rename columns
  df_one <- df_one %>%
    dplyr::mutate(
      ID_ONE := {{ id_one }},
      FORENAME_ONE := {{ forename_one }},
      SURNAME_ONE := {{ surname_one }},
      DOB_ONE := {{ dob_one }},
      POSTCODE_ONE := {{ postcode_one }}
    )

  # Rename columns
  df_two <- df_two %>%
    dplyr::mutate(
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
      format_postcode(., ID_ONE, POSTCODE_ONE) %>%
      format_name(., FORENAME_ONE) %>%
      format_name(., SURNAME_ONE) %>%
      format_date(., DOB_ONE)

    # Format df two
    df_two <- df_two %>%
      format_postcode(., ID_TWO, POSTCODE_TWO) %>%
      format_name(., FORENAME_TWO) %>%
      format_name(., SURNAME_TWO) %>%
      format_date(., DOB_TWO)
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
    calc_permutations(., FORENAME_ONE, SURNAME_ONE, POSTCODE_ONE, DOB_ONE)

  # Select columns and calculate permutations
  df_two <- df_two %>%
    calc_permutations(., FORENAME_TWO, SURNAME_TWO, POSTCODE_TWO, DOB_TWO)

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
    purrr::reduce(function(x, y) dplyr::bind_rows(x, y)) %>%
    dplyr::distinct()


  # Generate list of feasible dob-pairs with 6 identical characters
  cross <- id_pairs %>%
    filter_name(., FORENAME_ONE, FORENAME_TWO) %>%
    filter_dob(., DOB_ONE, DOB_TWO, 0.75)

  # Generate a list
  matches <- cross %>%
    dplyr::mutate(
      # If single character forename handle differently, otherwise JW
      JW_FORENAME = dplyr::case_when(
        length(FORENAME_ONE) == 1 & FORENAME_ONE == substr(FORENAME_TWO, 1, 1) ~ 0.75,
        FORENAME_ONE == FORENAME_TWO ~ 1,
        T ~ stringdist::stringsim(FORENAME_ONE, FORENAME_TWO, method = "jw", p = 0.1)
      )
    ) %>%
    dplyr::filter(JW_FORENAME >= 0.75) %>%
    dplyr::mutate(
      # JW match, bypassing exact string matches (DOB_SCORE already calculated)
      JW_SURNAME = ifelse(SURNAME_ONE == SURNAME_TWO, 1, stringdist::stringsim(SURNAME_ONE, SURNAME_TWO, method = "jw", p = 0.1)),
      JW_POSTCODE = ifelse(POSTCODE_ONE == POSTCODE_TWO, 1, stringdist::stringsim(POSTCODE_ONE, POSTCODE_TWO, method = "jw", p = 0.1)),
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
    dplyr::union_all(exact_matches)

  # Determine non-matches
  non_matches <- df_one %>%
    dplyr::anti_join(y = matches %>% dplyr::select(ID_ONE), by = "ID_ONE")

  # Final cross-join tally
  non_match_rows <- nrow(non_matches) / 1000
  match_rows <- nrow(df_one) / 1000

  # Cross-join size in 1000s
  cross_join_size <- non_match_rows * match_rows

  # Less than 1 billion then attempt cross join
  if (cross_join_size > 0 & cross_join_size <= 10) {

    # Message
    print("Final cross-join performed to maximise potential matches as would require less than 10m rows")
    #print(paste0("Cross-join size: ", cross_join_size, " million rows"))

    # Final cross-join matches, with corss-join threshold in place
    final_matches <- non_matches %>%
      dplyr::cross_join(
        y = df_two %>%
          dplyr::select(all_of(df_two_cols))
      ) %>%
      dplyr::mutate(
        # If single character forename handle differently, otherwise JW
        JW_FORENAME = dplyr::case_when(
          length(FORENAME_ONE) == 1 & FORENAME_ONE == substr(FORENAME_TWO, 1, 1) ~ 0.75,
          FORENAME_ONE == FORENAME_TWO ~ 1,
          T ~ stringdist::stringsim(FORENAME_ONE, FORENAME_TWO, method = "jw", p = 0.1)
        )
      ) %>%
      dplyr::filter(JW_FORENAME >= 0.75) %>%
      filter_dob(., DOB_ONE, DOB_TWO, 0.75)

    if (nrow(final_matches) > 0) {
      final_matches <- final_matches %>%
        dplyr::mutate(
          # JW match, bypassing exact string matches (DOB_SCORE already calculated)
          JW_SURNAME = ifelse(SURNAME_ONE == SURNAME_TWO, 1, stringdist::stringsim(SURNAME_ONE, SURNAME_TWO, method = "jw", p = 0.1)),
          JW_POSTCODE = ifelse(POSTCODE_ONE == POSTCODE_TWO, 1, stringdist::stringsim(POSTCODE_ONE, POSTCODE_TWO, method = "jw", p = 0.1)),
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
        ))
    } else {
      # create empty fields for missing match output
      # align structure of final_matches to that of matches
      final_matches <- final_matches %>%
        dplyr::mutate(JW_SURNAME = as.numeric(NA),
                      JW_POSTCODE = as.numeric(NA),
                      MATCH_TYPE = as.character(NA),
                      MATCH_SCORE = as.numeric(NA)
        )
    }

    # Re-determine non-matches
    non_matches <- non_matches %>%
      dplyr::anti_join(y = final_matches %>% dplyr::select(ID_ONE), by = "ID_ONE")

    # Re-determine total matches df
    matches <- matches %>%
      # Add exact matches
      dplyr::union_all(final_matches)
  } else {
    # at this point no cross-join  is required as either all matched or would be too many records
    if(cross_join_size > 10){
      # Message if the cross-join is not viable
      print("Final cross-join not viable as would require produce dataset greater than 10m rows")
      print(paste0("Cross-join size: ", cross_join_size, " million rows"))
    }

  }

  # Determine missing non-match fields
  non_matches <- non_matches %>%
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
      MATCH_SCORE = 0
    )

  # trim matches and non_matches to only include required columns
  key_columns = c("ID_ONE", "FORENAME_ONE", "SURNAME_ONE", "DOB_ONE", "POSTCODE_ONE",
                  "ID_TWO", "FORENAME_TWO", "SURNAME_TWO", "DOB_TWO", "POSTCODE_TWO",
                  "JW_FORENAME", "JW_SURNAME", "JW_POSTCODE", "DOB_SCORE", "MATCH_TYPE", "MATCH_SCORE")
  matches <- matches %>% dplyr::select(all_of(key_columns))
  non_matches <- non_matches %>% dplyr::select(all_of(key_columns))

  # combine the matches and non-matches if determined by the function parameter
  # handle cases where either of the datasets may be empty
  if (inc_no_match == FALSE) {
    all_matches <- matches
  } else if (nrow(matches) > 0 & nrow(non_matches) > 0) {
    all_matches <- matches %>%
      dplyr::union_all(non_matches)
  } else if (nrow(matches) > 0 & nrow(non_matches) == 0) {
    all_matches <- matches
  } else if (nrow(matches) == 0 & nrow(non_matches) > 0) {
    all_matches <- non_matches
  }

  # Calculate match_count per primary df ID
  all_matches <- all_matches %>%
    dplyr::group_by(ID_ONE) %>%
    dplyr::mutate(MATCH_COUNT = dplyr::n_distinct(ID_TWO)) %>%
    dplyr::ungroup() %>%
    # hard code "No Match" to a MATCH_COUNT of 0
    dplyr::mutate(MATCH_COUNT = ifelse(MATCH_TYPE == "No Match", 0, MATCH_COUNT))

  # Determine final output format
  if (output_type == "key") {

    # Only select key columns
    all_matches <- all_matches %>%
      dplyr::select(
        DF1_INPUT_ID := ID_ONE,
        DF2_INPUT_ID := ID_TWO,
        MATCH_TYPE,
        MATCH_COUNT,
        MATCH_SCORE
      )
  } else if (output_type == "match") {

    # Only select key columns
    all_matches <- all_matches %>%
      dplyr::select(
        DF1_INPUT_ID := ID_ONE,
        DF1_INPUT_FORENAME := FORENAME_ONE,
        DF1_INPUT_SURNAME := SURNAME_ONE,
        DF1_INPUT_DOB := DOB_ONE,
        DF1_INPUT_POSTCODE := POSTCODE_ONE,
        DF2_INPUT_ID := ID_TWO,
        DF2_INPUT_FORENAME := FORENAME_TWO,
        DF2_INPUT_SURNAME := SURNAME_TWO,
        DF2_INPUT_DOB := DOB_TWO,
        DF2_INPUT_POSTCODE := POSTCODE_TWO,
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
          dplyr::select(
            -{{ id_one }}, -{{ forename_one }}, -{{ surname_one }}, -{{ dob_one }}, -{{ postcode_one }},
            -FORENAME_ONE, -SURNAME_ONE, -DOB_ONE, -POSTCODE_ONE
          ) %>%
          dplyr::rename_all(list(~ paste0("DF1_", .))),
        by = c("ID_ONE" = "DF1_ID_ONE")
      ) %>%
      dplyr::left_join(
        df_two %>%
          dplyr::select(
            -{{ forename_two }}, -{{ surname_two }}, -{{ dob_two }}, -{{ postcode_two }},
            -PERM1, -PERM2, -PERM3, -PERM4, -PERM5, -PERM6, -PERM7, -PERM8, -PERM9
          ) %>%
          dplyr::rename_all(list(~ paste0("DF2_", .))),
        by = c("ID_TWO" = "DF2_ID_TWO")
      )

    # rename to match with input
    all_matches <- all_matches %>%
      dplyr::select(
        DF1_INPUT_ID := ID_ONE,
        DF1_INPUT_FORENAME := FORENAME_ONE,
        DF1_INPUT_SURNAME := SURNAME_ONE,
        DF1_INPUT_DOB := DOB_ONE,
        DF1_INPUT_POSTCODE := POSTCODE_ONE,
        DF2_INPUT_ID := ID_TWO,
        DF2_INPUT_FORENAME := FORENAME_TWO,
        DF2_INPUT_SURNAME := SURNAME_TWO,
        DF2_INPUT_DOB := DOB_TWO,
        DF2_INPUT_POSTCODE := POSTCODE_TWO,
        MATCH_TYPE,
        MATCH_COUNT,
        MATCH_SCORE,
        dplyr::everything()
      )
  }

  # Return data
  return(all_matches)
}
