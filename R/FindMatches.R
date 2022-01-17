#' FindMatches
#' Find all potential matches between two datasets
#'
#' @param df1 A dataframe containing person level information
#' @param df2 A dataframe containing person level information
#'
#' @return dataframe comprising of all potential matches between the two datasets
#' @export
#'
#' @examples
#' FindMatches(df1, df2)
FindMatches <- function(df1, df2) {

  # limit the fields from each dataset to only the key fields: ID, SURNAME, FORENAME, DOB, POSTCODE
  df1 <- df1 %>%
    dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
    dplyr::mutate(SURNAME = as.character(FormatName(SURNAME)),
                  FORENAME = as.character(FormatName(FORENAME)),
                  DOB = as.character(FormatDOB(DOB)),
                  POSTCODE = as.character(FormatPostcode(POSTCODE))
    )


  df2 <- df2 %>%
    dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
    dplyr::mutate(SURNAME = as.character(FormatName(SURNAME)),
                  FORENAME = as.character(FormatName(FORENAME)),
                  DOB = as.character(FormatDOB(DOB)),
                  POSTCODE = as.character(FormatPostcode(POSTCODE))
    )


  # identify exact matches
  df_exact <- df1 %>%
    # join where all fields match exactly
    dplyr::inner_join(df2, by = c("SURNAME" = "SURNAME", "FORENAME" = "FORENAME", "DOB" = "DOB", "POSTCODE" = "POSTCODE"), keep = FALSE) %>%
    # incorporate an additional join allowing the surname and forename to be flipped
    dplyr::bind_rows(inner_join(df1, df2, by = c("SURNAME" = "FORENAME", "FORENAME" = "SURNAME", "DOB" = "DOB", "POSTCODE" = "POSTCODE"), keep = FALSE)) %>%
    # by default all matches are an "exact" match with perfect scores
    dplyr::mutate(JW_SURNAME = 1, JW_FORENAME = 1, JW_POSTCODE = 1, ED_DOB = 0, MATCH_TYPE = 'Exact') %>%
    # select only the key fields
    dplyr::select(ID.x, ID.y, JW_SURNAME, JW_FORENAME, JW_POSTCODE, ED_DOB, MATCH_TYPE)


  # identify a list of records where an exact match has been identified
  exact_match_list <- unique(df_exact$ID.x)


  # combine the two datasets (basic cross join) for any records not already identified via exact matches
  df_combined <- df1 %>%
    # limit to only records not already matched based on an exact match
    dplyr::filter(!ID %in% exact_match_list) %>%
    # join the second dataset
    dplyr::full_join(df2, by = character(), suffix = c(".x", ".y")) %>%
    # perform string matching
    dplyr::mutate(JW_SURNAME = 1-stringdist::stringdist(SURNAME.x, SURNAME.y, method = "jw"),
                  JW_FORENAME = 1-stringdist::stringdist(FORENAME.x, FORENAME.y, method = "jw"),
                  JW_POSTCODE = 1-stringdist::stringdist(POSTCODE.x, POSTCODE.y, method = "jw"),
                  ED_DOB = stringdist::stringdist(DOB.x, DOB.y, method = "lv")
    ) %>%
    # limit to key fields and score matches
    dplyr::select(ID.x, ID.y, JW_SURNAME, JW_FORENAME, JW_POSTCODE, ED_DOB) %>%
    dplyr::mutate(MATCH_TYPE = case_when((JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB == 0) ~ "Exact",
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


  # combine the exact and confident matches
  df_match_results <- rbind(df_exact, df_combined)


  # identify the number of potential matches
  df_match_count <- df_match_results %>%
    dplyr::group_by(ID.x) %>%
    dplyr::summarise(MATCH_COUNT = n()) %>%
    dplyr::ungroup()


  # link the datasets back together
  df_match_results <- df_match_results %>%
    dplyr::inner_join(df_match_count, by = "ID.x") %>%
    dplyr::select(ID.x, MATCH_COUNT, ID.y, MATCH_TYPE)


  # return result
  return(df_match_results)
}
