#' Find all potential matches between two datasets
#'
#' @param df1 A dataframe containing person level information
#' @param df2 A dataframe containing person level information
#' @param inc_no_match  TRUE/FALSE(default) argument to specify if non-matches should be flagged
#' @param sw_forename proportion weighting value (0.0-1.0) to be applied to the forename part of the match score. Defaults to 0.3.
#' @param sw_surname proportion weighting value (0.0-1.0) to be applied to the surname part of the match score. Defaults to 0.15.
#' @param sw_dob proportion weighting value (0.0-1.0) to be applied to the dob part of the match score. Defaults to 0.4.
#' @param sw_postcode proportion weighting value (0.0-1.0) to be applied to the postcode part of the match score. Defaults to 0.15.
#'
#' @return dataframe comprising of all potential matches between two datasets
#' @export
#'
#' @examples
#' find_matches(df1, df2)
#' find_matches(df1, df2, inc_no_match = TRUE)
#' find_matches(df1, df2, TRUE)
find_matches <- function(df1, df2, inc_no_match = FALSE, sw_forename = 0.3, sw_surname = 0.15, sw_dob = 0.4, sw_postcode = 0.15) {

  # check if the match score weightings add up to 100% or any of the parameters have been entered as a non numeric value
  if(!is.numeric(sw_forename) || !is.numeric(sw_surname) || !is.numeric(sw_dob) || !is.numeric(sw_postcode)) {
    # non numeric value supplied as weighting factor
    stop("Non numeric value supplied as weighting factor", call. = FALSE)
  } else if(!dplyr::between(sw_forename,0,1) || !dplyr::between(sw_surname,0,1) || !dplyr::between(sw_dob,0,1) || !dplyr::between(sw_postcode,0,1)){
    # invalid values applied
    stop("Individual field score weighting values must be between 0.0 and 1.0", call. = FALSE)
  }else if((sw_forename + sw_surname + sw_dob + sw_postcode)!=1) {
    # if the proportions do not equal 100% abort the process and show an error message
    stop("Supplied score weighting values do not total 100%", call. = FALSE)
  }

  # Select and format relevant fields: ID, SURNAME, FORENAME, DOB, POSTCODE
  df1 <- df1 %>%
    dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
    dplyr::mutate(
      SURNAME = format_name(SURNAME),
      FORENAME = format_name(FORENAME),
      DOB = purrr::map_chr(DOB, format_dob),
      POSTCODE = purrr::map_chr(POSTCODE, format_postcode)
    )

  # Select and format relevant fields: ID, SURNAME, FORENAME, DOB, POSTCODE
  df2 <- df2 %>%
    dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
    dplyr::mutate(
      SURNAME = format_name(SURNAME),
      FORENAME = format_name(FORENAME),
      DOB = purrr::map_chr(DOB, format_dob),
      POSTCODE = purrr::map_chr(POSTCODE, format_postcode)
    )

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

  # Bind rows of both exact match df types
  df_exact <- df_exact %>%
    dplyr::bind_rows(df_exact_rev) %>%
    # by default all matches are an "exact" match with perfect scores
    dplyr::mutate(
      JW_SURNAME = 1,
      JW_FORENAME = 1,
      JW_POSTCODE = 1,
      ED_DOB = 1,
      MATCH_TYPE = 'Exact',
      MATCH_SCORE = 1
    ) %>%
    # select only the key fields
    dplyr::select(ID.x, ID.y, JW_SURNAME, JW_FORENAME, JW_POSTCODE, ED_DOB, MATCH_TYPE, MATCH_SCORE)

  # Identify a vector of records where an exact match has been identified
  exact_match_list <- df_exact %>%
    dplyr::select(ID.x) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # combine the two datasets (basic cross join) for non-exact matches records
  df_combined <- df1 %>%
    # limit to only records not already matched based on an exact match
    dplyr::filter(!ID %in% exact_match_list) %>%
    # join the second dataset
    dplyr::full_join(df2, by = character(), suffix = c(".x", ".y")) %>%
    # perform string matching
    dplyr::mutate(
      JW_SURNAME = stringdist::stringsim(SURNAME.x, SURNAME.y, method = "jw"),
      JW_FORENAME = stringdist::stringsim(FORENAME.x, FORENAME.y, method = "jw"),
      JW_POSTCODE = stringdist::stringsim(POSTCODE.x, POSTCODE.y, method = "jw"),
      ED_DOB = stringdist::stringsim(DOB.x, DOB.y, method = "lv")
    ) %>%
    # limit to key fields and score matches
    dplyr::select(ID.x, ID.y, JW_SURNAME, JW_FORENAME, JW_POSTCODE, ED_DOB) %>%
    dplyr::mutate(MATCH_TYPE = dplyr::case_when(
      (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB == 1) ~ "Exact",
      (JW_SURNAME == 1 & JW_FORENAME == 1 & ED_DOB == 1) ~ "Confident",
      (JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB >= 0.75) ~ "Confident",
      (JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB == 1) ~ "Confident",
      (JW_SURNAME == 1 & JW_FORENAME >= 0.75 & JW_POSTCODE == 1 & ED_DOB == 1) ~ "Confident",
      (JW_SURNAME >= 0.85 & JW_FORENAME >= 0.75 & JW_POSTCODE >= 0.85 & ED_DOB >= 0.75) ~ "Confident",
      TRUE ~ "No Match"
    )
    ) %>%
    # filter to only confident matches
    dplyr::filter(MATCH_TYPE != "No Match") %>%
    # calculate an overall weighted score
    # DOB attracts the highest weighting 40% as the least likely to change
    # forename attracts a waiting of 30% as this could be shortened
    # surname and DOB attract lowest weighting of 15% each as could be impacted by marriage/relocation
    dplyr::mutate(MATCH_SCORE = ((ifelse(is.na(JW_FORENAME), 0, JW_FORENAME) * sw_forename) +
                                   (ifelse(is.na(JW_SURNAME), 0, JW_SURNAME) * sw_surname) +
                                   (ifelse(is.na(ED_DOB), 0, ED_DOB) * sw_dob) +
                                   (ifelse(is.na(JW_POSTCODE), 0, JW_POSTCODE) * sw_postcode)
                                 ))

  # combine the exact and confident matches
  df_match_results <- rbind(df_exact, df_combined)

  # identify the number of potential matches
  df_match_count <- df_match_results %>%
    dplyr::group_by(ID.x) %>%
    dplyr::summarise(MATCH_COUNT = dplyr::n()) %>%
    dplyr::ungroup()

  # link the datasets back together
  df_match_results <- df_match_results %>%
    dplyr::inner_join(df_match_count, by = "ID.x") %>%
    dplyr::select(ID.x, MATCH_COUNT, ID.y, MATCH_TYPE, MATCH_SCORE)

  # if the user has requested non-matches included in the output
  # append these to the result set
  if(inc_no_match){
    # start with all records in the initial data-set
    df_no_match <- df1 %>%
      # join to the match results
      dplyr::select(ID.x = ID)%>%
      dplyr::left_join(df_match_results, by = "ID.x")%>%
      # remove anything with a match
      dplyr::filter(is.na(MATCH_COUNT)) %>%
      # populate the default values for the non matches
      dplyr::mutate(MATCH_COUNT = 0,
                    ID.y = "na",
                    MATCH_TYPE = "No Match",
                    MATCH_SCORE = 0
      )

    # combine the data-sets
    df_match_results <- rbind(df_match_results, df_no_match)
  }

  # return result
  return(df_match_results)
}
