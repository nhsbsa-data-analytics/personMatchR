#' Find all potential matches between two datasets
#'
#' @param df1 A dataframe containing person level information
#' @param df2 A dataframe containing person level information
#'
#' @return dataframe comprising of all potential matches between two datasets
#' @export
#'
#' @examples
#' find_matches(df1, df2)
find_matches <- function(df1, df2) {

  # Select and format relevant fields: ID, SURNAME, FORENAME, DOB, POSTCODE
  df1 <- df1 %>%
    dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
    dplyr::mutate(
      SURNAME = format_name(SURNAME),
      FORENAME = format_name(FORENAME),
      DOB = format_dob(DOB),
      POSTCODE = format_postcode(POSTCODE)
      )

  # Select and format relevant fields: ID, SURNAME, FORENAME, DOB, POSTCODE
  df2 <- df2 %>%
    dplyr::select(ID, SURNAME, FORENAME, DOB, POSTCODE) %>%
    dplyr::mutate(
    SURNAME = format_name(SURNAME),
    FORENAME = format_name(FORENAME),
    DOB = format_dob(DOB),
    POSTCODE = format_postcode(POSTCODE)
    )

  # combine the two datasets (basic cross join)
  df_combined <- dplyr::full_join(
    x = df1,
    y = df2,
    by = character(),
    suffix = c(".x", ".y")
    )

  # perform string matching
  df_combined <- df_combined %>%
    dplyr::mutate(
      JW_SURNAME = stringdist::stringsim(SURNAME.x, SURNAME.y, method = "jw"),
      JW_FORENAME = stringdist::stringsim(FORENAME.x, FORENAME.y, method = "jw"),
      JW_POSTCODE = stringdist::stringsim(POSTCODE.x, POSTCODE.y, method = "jw"),
      ED_DOB = stringdist::stringdist(DOB.x, DOB.y, method = "lv")
    ) %>%
    # score matches
    select(ID.x, ID.y, JW_SURNAME, JW_FORENAME, JW_POSTCODE, ED_DOB) %>%
    dplyr::mutate(MATCH_TYPE = case_when(
      JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB == 0 ~ "Exact",
      JW_SURNAME == 1 & JW_FORENAME == 1 & ED_DOB == 0 ~ "Confident",
      JW_SURNAME == 1 & JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB <= 2 ~ "Confident",
      JW_FORENAME == 1 & JW_POSTCODE == 1 & ED_DOB == 0 ~ "Confident",
      JW_SURNAME == 1 & JW_FORENAME >= 0.75 & JW_POSTCODE == 1 & ED_DOB == 0 ~ "Confident",
      JW_SURNAME >= 0.85 & JW_FORENAME >= 0.75 & JW_POSTCODE >= 0.85 & ED_DOB <= 2 ~ "Confident",
      TRUE ~ "No Match"
      )
    ) %>%
    # filter to only confident matches
    dplyr::filter(MATCH_TYPE != "No Match")

  # Identify the number of potential matches
  df_match_count <- df_combined %>%
    dplyr::group_by(ID.x) %>%
    dplyr::summarise(MATCH_COUNT = n()) %>%
    dplyr::ungroup()

  # Link the datasets back together
  df_combined <- dplyr::inner_join(df_combined, df_match_count, by = "ID.x") %>%
    dplyr::select(ID.x, MATCH_COUNT, ID.y, MATCH_TYPE)

  # return result
  return(df_combined)
}
