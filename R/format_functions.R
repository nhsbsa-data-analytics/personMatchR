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
format_dob <- function(df, dob){

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

#' Calculates 9 permutations for primary-lookup join prior to match scoring
#'
#' @param df A df to be formatted
#' @param forename forename column name
#' @param surname surname column name
#' @param postcode postcode column name
#' @param dob DOB column name
#'
#' @return A df with 5 'basic' join permutations added
#'
#' @export
#'
#' @examples
#' calc_permutations(df, forename, surname, postcode, dob)
calc_permutations <- function(df, forename, surname, postcode, dob){

  df %>%
    dplyr::mutate(
      # Perm 1-3 require full match of dob with 2 of forename, surname, postcode
      PERM1 = paste0({{ forename }}, {{ surname }}, {{ dob }}),
      PERM2 = paste0({{ forename }}, {{ postcode }}, {{dob}}),
      PERM3 = paste0({{ surname }}, {{ postcode }}, {{ dob }}),
      # First char forename - 4 chars of surname & postcode
      PERM4 = paste0(
        substr({{ forename }}, 1, 1),
        substr({{ surname }}, 1, 4),
        substr({{ postcode }}, 1, 4)
      ),
      # First 3 chars forename - 2 chars surname & postcode
      PERM5 = paste0(
        substr({{ forename }}, 1, 3),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # Last 3 chars forename - 3 chars of surname & postcode
      PERM6 = paste0(
        substr({{ forename }}, nchar({{ forename }})-2, 3),
        substr({{ surname }}, 1, 3),
        substr({{ postcode }}, 1, 3)
      ),
      # First 3 consonants - 3 chars of surname & postcode
      PERM7 = paste0(
        substr(gsub('[AEIOU]', '', {{ forename}}), 1, 3),
        substr({{ surname }}, 1, 3),
        substr({{ postcode }}, 1, 3)
      ),
      # All consonants - 2 chars of surname & postcode
      PERM8 = paste0(
        gsub('[AEIOU]', '', {{ forename}}),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # All vowels - 3 chars of surname & postcode
      PERM9 = paste0(
        gsub('[B-DF-HJ-NP-TV-Z]', '', {{ forename }}),
        substr({{ surname }}, 1, 3),
        substr({{ postcode }}, 1, 3)
      )
    )
}
