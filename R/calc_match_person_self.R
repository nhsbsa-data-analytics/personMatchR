#' Find all potential person matches within a single dataset based on personal identifiable information
#'
#' The input dataset should be dataframe, where for each person in the dataset the function will
#' try to identify any other records for the same individual.
#' \cr\cr Match results will include cases where the records match either exactly or confidently based on matching rules.
#' \cr\cr Parameters can be used to specify the formatting of the output dataset and allow some
#' customisation of the match scoring.
#'
#' @param df dataframe containing person level information representing people to look for
#' @param id unique reference column for df
#' @param forename forename column for df
#' @param surname surname column for df
#' @param dob date of birth column for df, ideally in dd/mm/yyyy format although others are handled
#' @param postcode postcode field for df
#' @param output_type One of the following: "key" / "match" / "all"
#' @param format_data TRUE/FALSE : identifying if the input datasets should be passed through
#' formatting functions to clean data prior to matching
#' @param inc_no_match TRUE/FALSE : identifying if the output should include non matches
#' @param unique_combinations_only TRUE/FALSE : identifying if the output should include both directions for combination pairs (e.g. A=B & B=A)
#' @param sw_forename (default = 0.30) proportion weighting value (0.0-1.0) to be applied to the forename part of the match score
#' @param sw_surname (default = 0.15) proportion weighting value (0.0-1.0) to be applied to the surname part of the match score
#' @param sw_dob (default = 0.40) proportion weighting value (0.0-1.0) to be applied to the dob part of the match score
#' @param sw_postcode (default = 0.15) proportion weighting value (0.0-1.0) to be applied to the postcode part of the match score
#'
#' @return dataframe comprising of all potential matches within the dataset
#' @export
#'
calc_match_person_self <- function(df, id, forename, surname, dob, postcode,
                              output_type = c("all", "key", "match"),
                              format_data = c(TRUE, FALSE),
                              inc_no_match = c(TRUE, FALSE),
                              unique_combinations_only = c(TRUE, FALSE),
                              sw_forename = 0.3, sw_surname = 0.15, sw_dob = 0.4, sw_postcode = 0.15) {

  # Match arguments
  match.arg(output_type)

  # Parameter Check ---------------------------------------------------------
  # check input parameters meet requirements

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

  # check if the dataset df supplied has any column names that may cause an issue
  # the parameters will define the key personal information fields
  # these will be translated for matching as define in error_cols below
  # the dataset cannot contain any fields with the same name as will be included

  # List of illegible names for non function-input columns
  error_cols <- c(
    "ID_ONE", "FORENAME_ONE", "SURNAME_ONE", "DOB_ONE", "POSTCODE_ONE",
    "ID_TWO", "FORENAME_TWO", "SURNAME_TWO", "DOB_TWO", "POSTCODE_TWO"
  )

  # identify all columns names from input df
  all_cols <- c(colnames(df))

  # List of function-input column names
  input_cols <- c(
    {
      deparse(substitute(id))
    },
    {
      deparse(substitute(forename))
    },
    {
      deparse(substitute(surname))
    },
    {
      deparse(substitute(dob))
    },
    {
      deparse(substitute(postcode))
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

  # Dataset Setup -----------------------------------------------------------
  # create the datasets that will be used for matching

  # Create version of dataset for left hand side of matching, renaming columns
  df_one <- df  |>
    dplyr::mutate(
      ID_ONE := {{ id }},
      FORENAME_ONE := {{ forename }},
      SURNAME_ONE := {{ surname }},
      DOB_ONE := {{ dob }},
      POSTCODE_ONE := {{ postcode }}
    )

  # Create version of dataset for right hand side of matching, renaming columns
  df_two <- df  |>
    dplyr::mutate(
      ID_TWO := {{ id }},
      FORENAME_TWO := {{ forename }},
      SURNAME_TWO := {{ surname }},
      DOB_TWO := {{ dob }},
      POSTCODE_TWO := {{ postcode }}
    )

  # Format data depending on function input selection
  if (format_data == TRUE) {

    # Format df one
    df_one <- format_postcode(df = df_one, id = ID_ONE, postcode = POSTCODE_ONE)
    df_one <- format_name(df = df_one, name = FORENAME_ONE)
    df_one <- format_name(df = df_one, name = SURNAME_ONE)
    df_one <- format_date(df = df_one, date = DOB_ONE)

    # Format df two
    df_two <- format_postcode(df = df_two, id = ID_TWO, postcode = POSTCODE_TWO)
    df_two <- format_name(df = df_two, name = FORENAME_TWO)
    df_two <- format_name(df = df_two, name = SURNAME_TWO)
    df_two <- format_date(df = df_two, date = DOB_TWO)

  }

  # Df column names
  df_one_cols <- colnames(df_one)
  df_two_cols <- colnames(df_two)

  # Exact Matches -----------------------------------------------------------
  # identify exact matches between the datasets

  # Identify exact matches (all key fields match)
  exact_matches <- df_one |>
    dplyr::inner_join(
      y = df_two,
      by = c(
        "FORENAME_ONE" = "FORENAME_TWO",
        "SURNAME_ONE" = "SURNAME_TWO",
        "DOB_ONE" = "DOB_TWO",
        "POSTCODE_ONE" = "POSTCODE_TWO"
      ),
      na_matches = "never",
      relationship = "many-to-many"
    )

  # Identify reverse exact matches (forename = surname & surname = forename)
  exact_matches_reverse <- df_one |>
    dplyr::inner_join(
      y = df_two,
      by = c(
        "FORENAME_ONE" = "SURNAME_TWO",
        "SURNAME_ONE" = "FORENAME_TWO",
        "DOB_ONE" = "DOB_TWO",
        "POSTCODE_ONE" = "POSTCODE_TWO"
      ),
      na_matches = "never",
      relationship = "many-to-many"
    )

  # Union exact matches to get a single dataset of matches and format output
  exact_matches <- exact_matches |>
    dplyr::union_all(exact_matches_reverse) |>
    dplyr::distinct() |>
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
    ) |>
    # exclude where the IDs match (prevent self-join)
    dplyr::filter(ID_ONE != ID_TWO)

  # Partial Match (permutation based) ---------------------------------------
  # identify matches where there is a close match between datasets

  # Add match permutations to the LHS dataset
  df_one <- calc_permutations(df_one, FORENAME_ONE, SURNAME_ONE, POSTCODE_ONE, DOB_ONE)

  # Add match permutations to the RHS dataset
  df_two <- calc_permutations(df_two, FORENAME_TWO, SURNAME_TWO, POSTCODE_TWO, DOB_TWO)

  # List of permutation-join columns
  perm_num <- paste0("PERM", 1:9)

  # Combine datasets based on any matching pair of permutation-join columns
  id_pairs <- perm_num |>
    purrr::map(~ {
      df_one |>
        dplyr::select(all_of(df_one_cols), {{ .x }}) |>
        dplyr::inner_join(
          df_two |>
            dplyr::select(all_of(df_two_cols), {{ .x }}),
          by = {{ .x }},
          relationship = "many-to-many"
        ) |>
        dplyr::select(-{{ .x }})
    }) |>
    purrr::reduce(function(x, y) dplyr::bind_rows(x, y)) |>
    dplyr::distinct()

  # Filter match pairs to remove previously identified exact matches
  id_pairs <- id_pairs |>
    dplyr::anti_join(y = exact_matches, by = c("ID_ONE" = "ID_ONE","ID_TWO" = "ID_TWO"))

  # exclude where the IDs match (prevent self-join)
  id_pairs <- id_pairs |>
    dplyr::filter(ID_ONE != ID_TWO)

  # Filter the potential matches to remove any that are unfeasible based on forename or DOB matches
  id_pairs <- filter_name(id_pairs, FORENAME_ONE, FORENAME_TWO)
  id_pairs <- filter_dob(id_pairs, DOB_ONE, DOB_TWO, 0.75)


  # Perform match scoring to limit to confident matches only
  matches <- id_pairs |>
    dplyr::mutate(
      # handle forename matching based on the length of the forename
      JW_FORENAME = dplyr::case_when(
        # If forenames match then score as 1
        FORENAME_ONE == FORENAME_TWO ~ 1,
        # for initial only forenames, if this matches first character then score as 0.75
        length(FORENAME_ONE) == 1 & FORENAME_ONE == substr(FORENAME_TWO, 1, 1) ~ 0.75,
        # otherwise use Jaro-Winkler string matching function
        T ~ stringdist::stringsim(FORENAME_ONE, FORENAME_TWO, method = "jw", p = 0.1)
      )
    ) |>
    # limit to matches with a minimum match score of 0.75 for forename
    dplyr::filter(JW_FORENAME >= 0.75) |>
    # score the surname and postcode matches and identify match type based on match rules
    dplyr::mutate(
      # If the surname/postcode are exact match score as 1 otherwise use Jaro-Winkler  string matching
      JW_SURNAME = ifelse(SURNAME_ONE == SURNAME_TWO, 1, stringdist::stringsim(SURNAME_ONE, SURNAME_TWO, method = "jw", p = 0.1)),
      JW_POSTCODE = ifelse(POSTCODE_ONE == POSTCODE_TWO, 1, stringdist::stringsim(POSTCODE_ONE, POSTCODE_TWO, method = "jw", p = 0.1)),
      # Flag the match type based on the combination of match scores for each of the key fields
      MATCH_TYPE = dplyr::case_when(
        (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DOB_SCORE == 1) ~ "Exact",
        (JW_SURNAME == 1 & JW_FORENAME == 1 & DOB_SCORE == 1) ~ "Confident",
        (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DOB_SCORE >= 0.75) ~ "Confident",
        (JW_FORENAME == 1 & JW_POSTCODE == 1 & DOB_SCORE == 1) ~ "Confident",
        (JW_SURNAME == 1 & JW_FORENAME >= 0.75 & JW_POSTCODE == 1 & DOB_SCORE == 1) ~ "Confident",
        (JW_SURNAME >= 0.9 & JW_FORENAME >= 0.9 & JW_POSTCODE >= 0.9 & DOB_SCORE == 1) ~ "Confident",
        TRUE ~ "No Match"
      )
    ) |>
    # filter to only confident matches
    dplyr::filter(MATCH_TYPE != "No Match") |>
    # calculate an overall weighted score
    dplyr::mutate(MATCH_SCORE = (
      (ifelse(is.na(JW_FORENAME), 0, JW_FORENAME) * sw_forename) +
        (ifelse(is.na(JW_SURNAME), 0, JW_SURNAME) * sw_surname) +
        (ifelse(is.na(DOB_SCORE), 0, DOB_SCORE) * sw_dob) +
        (ifelse(is.na(JW_POSTCODE), 0, JW_POSTCODE) * sw_postcode)
    )) |>
    # Add exact matches
    dplyr::union_all(exact_matches)

  # Determine non-matches
  non_matches <- df_one |>
    # drop the match permutations
    dplyr::select(-PERM1, -PERM2, -PERM3, -PERM4, -PERM5, -PERM6, -PERM7, -PERM8, -PERM9) |>
    dplyr::anti_join(y = matches |> dplyr::select(ID_ONE), by = "ID_ONE")


  # Additional Cross Join ---------------------------------------------------
  # depending on the dataset size it may be feasible to perform a cross join
  # this will join all LHS records to all RHS records to get maximum chance of matches
  # only feasible if this would produce less than 10m records to compare

  # Calculate the potential size of a cross join on non matched records
  cross_join_size <- (nrow(non_matches) / 1000) * (nrow(df_one) / 1000)

  # Less than 10 million then attempt cross join (as long as something left to join)
  if (cross_join_size > 0 & cross_join_size <= 10) {

    # Display update message to user
    print("Final cross-join performed to maximise potential matches as would require less than 10m rows")

    # Perform cross join to link remaining "non_matches" to all records in "df_two"
    cross_join_matches <- non_matches |>
      dplyr::cross_join(
        y = df_two |>
          dplyr::select(all_of(df_two_cols))
      ) |>
      # exclude where the IDs match (prevent self-join)
      dplyr::filter(ID_ONE != ID_TWO) |>
      dplyr::mutate(
        # handle forename matching based on the length of the forename
        JW_FORENAME = dplyr::case_when(
          # If forenames match then score as 1
          FORENAME_ONE == FORENAME_TWO ~ 1,
          # for initial only forenames, if this matches first character then score as 0.75
          length(FORENAME_ONE) == 1 & FORENAME_ONE == substr(FORENAME_TWO, 1, 1) ~ 0.75,
          # otherwise use Jaro-Winkler string matching function
          T ~ stringdist::stringsim(FORENAME_ONE, FORENAME_TWO, method = "jw", p = 0.1)
        )
      ) |>
      # limit to matches with a minimum match score of 0.75 for forename
      dplyr::filter(JW_FORENAME >= 0.75)

    # limit to records based on DOB comparison (up to two characters different)
    cross_join_matches <- filter_dob(cross_join_matches, DOB_ONE, DOB_TWO, 0.75)

    # Perform match scoring to limit to confident matches only
    if (nrow(cross_join_matches) > 0) {
      cross_join_matches <- cross_join_matches |>
        dplyr::mutate(
          # If the surname/postcode are exact match score as 1 otherwise use Jaro-Winkler  string matching
          JW_SURNAME = ifelse(SURNAME_ONE == SURNAME_TWO, 1, stringdist::stringsim(SURNAME_ONE, SURNAME_TWO, method = "jw", p = 0.1)),
          JW_POSTCODE = ifelse(POSTCODE_ONE == POSTCODE_TWO, 1, stringdist::stringsim(POSTCODE_ONE, POSTCODE_TWO, method = "jw", p = 0.1)),
          # Flag the match type based on the combination of match scores for each of the key fields
          MATCH_TYPE = dplyr::case_when(
            (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DOB_SCORE == 1) ~ "Exact",
            (JW_SURNAME == 1 & JW_FORENAME == 1 & DOB_SCORE == 1) ~ "Confident",
            (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & DOB_SCORE >= 0.75) ~ "Confident",
            (JW_FORENAME == 1 & JW_POSTCODE == 1 & DOB_SCORE == 1) ~ "Confident",
            (JW_SURNAME == 1 & JW_FORENAME >= 0.75 & JW_POSTCODE == 1 & DOB_SCORE == 1) ~ "Confident",
            (JW_SURNAME >= 0.9 & JW_FORENAME >= 0.9 & JW_POSTCODE >= 0.9 & DOB_SCORE == 1) ~ "Confident",
            TRUE ~ "No Match"
          )
        ) |>
        # filter to only confident matches
        dplyr::filter(MATCH_TYPE != "No Match") |>
        # calculate an overall weighted score
        dplyr::mutate(MATCH_SCORE = (
          (ifelse(is.na(JW_FORENAME), 0, JW_FORENAME) * sw_forename) +
            (ifelse(is.na(JW_SURNAME), 0, JW_SURNAME) * sw_surname) +
            (ifelse(is.na(DOB_SCORE), 0, DOB_SCORE) * sw_dob) +
            (ifelse(is.na(JW_POSTCODE), 0, JW_POSTCODE) * sw_postcode)
        ))
    } else {
      # create empty fields for missing match output
      # align structure of cross_join_matches to that of matches
      cross_join_matches <- cross_join_matches |>
        dplyr::mutate(JW_SURNAME = as.numeric(NA),
                      JW_POSTCODE = as.numeric(NA),
                      MATCH_TYPE = as.character(NA),
                      MATCH_SCORE = as.numeric(NA)
        )
    }

    # Re-determine non-matches
    non_matches <- non_matches |>
      dplyr::anti_join(y = cross_join_matches |> dplyr::select(ID_ONE), by = "ID_ONE")

    # Re-determine total matches df
    matches <- matches |>
      # Add cross-join matches
      dplyr::union_all(cross_join_matches)

  } else {
    # at this point no cross-join is required as either all matched or would be too many records
    if(cross_join_size > 10){
      # Message if the cross-join is not viable due to number of records
      print("Final cross-join not viable as would require produce dataset greater than 10m rows")
      print(paste0("Cross-join size: ", cross_join_size, " million rows"))
    }

  }

  # Determine missing non-match fields
  non_matches <- non_matches |>
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
  matches <- matches |> dplyr::select(all_of(key_columns))
  non_matches <- non_matches |> dplyr::select(all_of(key_columns))

  # Handle duplicate matches ------------------------------------------------
  # Matching within a single dataset will produce some duplication (A=B and B=A)
  # A parameter will define if these should be included or excluded from the output

  if (unique_combinations_only == TRUE) {
    # identify the unique pairs of records and retain only one of them
    matches <- matches |>
      dplyr::rowwise() |>
      dplyr::mutate(
        min_id = min(ID_ONE, ID_TWO),
        max_id = max(ID_ONE, ID_TWO),
      ) |>
      dplyr::ungroup() |>
      dplyr::group_by(min_id, max_id) |>
      dplyr::mutate(combo_id = dplyr::row_number(ID_ONE)) |>
      dplyr::ungroup() |>
      dplyr::filter(combo_id == 1) |>
      dplyr::select(-min_id, -max_id, -combo_id)
  }

  # Handle non-matches ------------------------------------------------------
  # combine the matches and non-matches if determined by the function parameter
  # handle cases where either of the datasets may be empty

  if (inc_no_match == FALSE) {
    all_matches <- matches
  } else if (nrow(matches) > 0 & nrow(non_matches) > 0) {
    all_matches <- matches |>
      dplyr::union_all(non_matches)
  } else if (nrow(matches) > 0 & nrow(non_matches) == 0) {
    all_matches <- matches
  } else if (nrow(matches) == 0 & nrow(non_matches) > 0) {
    all_matches <- non_matches
  }

  # Handle match output -----------------------------------------------------
  # format the final output based on parameters

  # Calculate match_count per primary df ID
  all_matches <- all_matches |>
    dplyr::group_by(ID_ONE) |>
    dplyr::mutate(MATCH_COUNT = dplyr::n_distinct(ID_TWO)) |>
    dplyr::ungroup() |>
    # hard code "No Match" to a MATCH_COUNT of 0
    dplyr::mutate(MATCH_COUNT = ifelse(MATCH_TYPE == "No Match", 0, MATCH_COUNT))

  # Determine final output format
  if (output_type == "key") {

    # Only select key columns
    all_matches <- all_matches |>
      dplyr::select(
        DF1_INPUT_ID := ID_ONE,
        DF2_INPUT_ID := ID_TWO,
        MATCH_TYPE,
        MATCH_COUNT,
        MATCH_SCORE
      )

  } else if (output_type == "match") {

    # Only select key columns
    all_matches <- all_matches |>
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
    all_matches <- all_matches |>
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

    # join back to input df to get any missing columns
    # remove any of the key columns from df_one and df_two to stop duplicates
    # prefix the additional columns with an identifier for the source dataset
    all_matches <- all_matches |>
      dplyr::left_join(
        df_one |>
          dplyr::select(
            -{{ id }}, -{{ forename }}, -{{ surname }}, -{{ dob }}, -{{ postcode }},
            -FORENAME_ONE, -SURNAME_ONE, -DOB_ONE, -POSTCODE_ONE
          ) |>
          dplyr::rename_all(list(~ paste0("DF1_", .))),
        by = c("ID_ONE" = "DF1_ID_ONE")
      ) |>
      dplyr::left_join(
        df_two |>
          dplyr::select(
            -{{ id }}, -{{ forename }}, -{{ surname }}, -{{ dob }}, -{{ postcode }},
            -FORENAME_TWO, -SURNAME_TWO, -DOB_TWO, -POSTCODE_TWO,
            -PERM1, -PERM2, -PERM3, -PERM4, -PERM5, -PERM6, -PERM7, -PERM8, -PERM9
          ) |>
          dplyr::rename_all(list(~ paste0("DF2_", .))),
        by = c("ID_TWO" = "DF2_ID_TWO")
      )

    # rename to match with input
    all_matches <- all_matches |>
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
