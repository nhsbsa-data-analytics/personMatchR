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
format_name <- function(df){

  df %>%
    mutate(
      # Remove non-alpha chars and convert emtpy string to NA
      FORENAME = toupper(gsub("[^[:alpha:]]", "", FORENAME)),
      FORENAME = ifelse(FORENAME == "", NA, FORENAME),
      SURNAME = toupper(gsub("[^[:alpha:]]", "", SURNAME)),
      FORENAME = ifelse(FORENAME == "", NA, FORENAME),
      SURNAME = ifelse(SURNAME == "", NA, SURNAME)
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
format_dob <- function(df){

  df %>%
    mutate(
      # Remove hyphens and upper case
      DOB = gsub('/', '-', DOB),
      DOB = toupper(DOB),
      # Generate various potential dates
      DOB_V1 = as.Date(DOB, format = '%d-%B-%y'),
      DOB_V2 = as.Date(DOB, format = '%Y-%m-%d'),
      # Find first non-NA date then format as string
      DOB = dplyr::coalesce(DOB_V1, DOB_V2),
      DOB = format(DOB, format = '%Y%m%d'),
      # Empty strings as NA
      DOB = ifelse(DOB == "", NA, DOB)
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
format_postcode = function(df){

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
    mutate(
      POSTCODE = ifelse(POSTCODE == "", NA, POSTCODE),
      POSTCODE = toupper(stringr::str_replace_all(POSTCODE, "[^[:alnum:]]", ""))
    ) %>%
    tidytext::unnest_characters(., 'CHAR', 'POSTCODE', to_lower = F) %>%
    mutate(NUM = ifelse(grepl("[1-9]", CHAR), 1, 0)) %>%
    group_by(ID) %>%
    # Determine total chars and char position per postcode
    mutate(
      ROW = row_number(),
      LEN = max(ROW)
    ) %>%
    ungroup() %>%
    # Apply help functions depending on postcode length and char position
    mutate(
      CHAR = case_when(
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
    select(-c(NUM, LEN)) %>%
    group_by(ID) %>%
    # Paste split chars into single column
    mutate(POSTCODE = paste(CHAR, collapse = "")) %>%
    ungroup() %>%
    # Select and distinct to remove split-char rows
    select(ID, FORENAME, SURNAME, DOB, POSTCODE) %>%
    distinct()
}
