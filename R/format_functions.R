#' Formatting either a Forename or Surname
#'
#' Format the name strings prior to matching
#' Formatting includes conversion to upper case and removal of
#' non alphabetic characters
#'
#' @param str_name A string field to be cleansed
#'
#' @return A cleansed string
#'
#' @export
#'
#' @examples
#' format_name(str_name)
format_name <- function(df, name){

  df %>%
    dplyr::mutate(
      # Remove non-alpha chars and convert emtpy string to NA
      {{ name }} := toupper(gsub("[^[:alpha:]]", "", {{ name }})),
      {{ name }} := ifelse({{ name }} == "", NA, {{ name }})
    )
}

#' Format a DOB
#'
#' Format the date of birth prior to matching
#' Convert to a string representation in the format YYYYMMDD
#'
#' @param df A dataframe with date of birth date field to be cleansed
#'
#' @return A dataframe with cleansed date of birth
#'
#' @export
#'
#' @examples
#' format_dob(dob)
format_date <- function(df, dob){

  df %>%
    dplyr::mutate(
      # Remove hyphens and upper case
      {{ dob }} := gsub('/', '-', {{ dob }}),
      {{ dob }} := toupper({{ dob }}),
      # Generate various potential dates
      DOB_V1 = as.Date({{ dob }}, format = '%d-%B-%y'),
      DOB_V2 = as.Date({{ dob }}, format = '%Y-%m-%d'),
      # Find first non-NA date then format as string
      {{ dob }} := dplyr::coalesce(DOB_V1, DOB_V2),
      {{ dob }} := format({{ dob }}, format = '%Y%m%d'),
      # Empty strings as NA
      {{ dob }} := ifelse({{ dob }} == "", NA, {{ dob }})
    ) %>%
    select(-c(DOB_V1, DOB_V2))
}

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
format_postcode = function(df, id, postcode){

  # Helper function to replace numbers for characters
  replace_number_for_char = function(x){
    stringr::str_replace_all(x, c("0" = "O", "5" = "S"))
  }

  # Helper function to replace characters for numbers
  replace_char_for_number = function(x){
    stringr::str_replace_all(x, c("O" = "0", "I" = "1", "L" = "1", "S" = "5"))
  }

  # Process df
  df %>%
    dplyr::mutate(
      {{ postcode }} := toupper(gsub("[^[:alnum:]]", "", {{ postcode }}))
    ) %>%
    tidytext::unnest_characters(., 'CHAR', {{ postcode }}, to_lower = F) %>%
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
    dplyr::distinct()
}
