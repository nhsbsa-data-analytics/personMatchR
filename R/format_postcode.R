#' Format the postcode strings prior to matching
#'
#' Formatting includes conversion to upper case and removal of
#' non alphanumeric characters
#'
#' @param df A dataframe to feed into function
#'
#' @return A dataframe with cleansed postcode information
#' @export
#'
#' @examples
#' format_postcode(df)
format_postcode <- function(df, id, postcode) {

  # Helper function to replace numbers for characters
  replace_number_for_char <- function(x) {
    stringr::str_replace_all(x, c("0" = "O", "5" = "S"))
  }

  # Helper function to replace characters for numbers
  replace_char_for_number <- function(x) {
    stringr::str_replace_all(x, c("O" = "0", "I" = "1", "L" = "1", "S" = "5"))
  }

  # Process df
  df %>%
    dplyr::mutate(
      {{ postcode }} := toupper(gsub("[^[:alnum:]]", "", {{ postcode }}))
    ) %>%
    # convert blank strings ("") to NA
    dplyr::mutate(
      {{ postcode }} := na_if({{ postcode }}, "")
    ) %>%
    tidytext::unnest_characters(., "CHAR", {{ postcode }}, to_lower = F) %>%
    dplyr::mutate(NUM = ifelse(grepl("[1-9]", CHAR), 1, 0)) %>%
    dplyr::group_by({{ id }}) %>%
    # Determine total chars and char position per postcode
    dplyr::mutate(
      ROW = row_number(),
      LEN = max(ROW)
    ) %>%
    dplyr::ungroup() %>%
    # Apply help functions depending on postcode length and char position
    dplyr::mutate(
      CHAR = dplyr::case_when(
        # Postcode Length 7
        LEN == 7 & ROW == 1 ~ replace_number_for_char(CHAR),
        LEN == 7 & ROW == 2 ~ replace_number_for_char(CHAR),
        LEN == 7 & ROW == 3 ~ replace_char_for_number(CHAR),
        LEN == 7 & ROW == 5 ~ replace_char_for_number(CHAR),
        LEN == 7 & ROW == 6 ~ replace_number_for_char(CHAR),
        LEN == 7 & ROW == 7 ~ replace_number_for_char(CHAR),
        # Postcode Length 6
        LEN == 6 & ROW == 1 ~ replace_number_for_char(CHAR),
        LEN == 6 & ROW == 4 ~ replace_char_for_number(CHAR),
        LEN == 6 & ROW == 5 ~ replace_number_for_char(CHAR),
        LEN == 6 & ROW == 6 ~ replace_number_for_char(CHAR),
        # Postode Length 5
        LEN == 5 & ROW == 1 ~ replace_number_for_char(CHAR),
        LEN == 5 & ROW == 2 ~ replace_char_for_number(CHAR),
        LEN == 5 & ROW == 3 ~ replace_char_for_number(CHAR),
        LEN == 5 & ROW == 4 ~ replace_number_for_char(CHAR),
        LEN == 5 & ROW == 5 ~ replace_number_for_char(CHAR),
        # Postcode Remaining
        T ~ CHAR
      )
    ) %>%
    dplyr::select(-c(NUM, LEN)) %>%
    dplyr::group_by({{ id }}) %>%
    # Paste split chars into single column
    dplyr::mutate({{ postcode }} := paste(CHAR, collapse = "")) %>%
    dplyr::ungroup() %>%
    # Select and distinct to remove split-char rows
    dplyr::select(-c(CHAR, ROW)) %>%
    dplyr::distinct() %>%
    # convert "NA" to NA
    dplyr::mutate(
      {{ postcode }} := na_if({{ postcode }}, "NA")
    )
}
