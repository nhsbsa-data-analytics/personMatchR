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
    mutate(
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
    mutate(
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
    mutate(
      {{ postcode }} := toupper(gsub("[^[:alnum:]]", "", {{ postcode }}))
    ) %>%
    tidytext::unnest_characters(., 'CHAR', {{ postcode }}, to_lower = F) %>%
    mutate(NUM = ifelse(grepl("[1-9]", CHAR), 1, 0)) %>%
    group_by({{ id }}) %>%
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
    group_by({{ id }}) %>%
    # Paste split chars into single column
    mutate({{ postcode }} := paste(CHAR, collapse = "")) %>%
    ungroup() %>%
    # Select and distinct to remove split-char rows
    select(-c(CHAR, ROW)) %>%
    distinct()
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
    mutate(
      # Perm 1-3 require full match of dob with 2 of forename, surname, postcode
      PERM1 = paste0({{ forename }}, {{ surname }}, {{ dob }}),
      PERM2 = paste0({{ forename }}, {{ postcode }}, {{dob}}),
      PERM3 = paste0({{ surname }}, {{ postcode }}, {{ dob }}),
      # First char forename, plus 4 chars of surname and postcode
      PERM4 = paste0(
        substr({{ forename }}, 1, 1),
        substr({{ surname }}, 1, 4),
        substr({{ postcode }}, 1, 4)
      ),
      # First 3 chars forename, plus 3 chars of surname and postcode
      PERM5 = paste0(
        substr({{ forename }}, 1, 3),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # Last 3 chars forename, plus 3 chars of surname and postcode
      PERM6 = paste0(
        substr({{ forename }}, nchar({{ forename }})-2, 3),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # First 3 consonants, plus 3 chars of surname and postcode
      PERM7 = paste0(
        substr(gsub('[AEIOU]', '', {{ forename}}), 1, 3),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # All consonants, plus 4 chars of surname and postcode
      PERM8 = paste0(
        gsub('[AEIOU]', '', {{ forename}}),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # All vowels, plus 3 chars of surname and postcode
      PERM9 = paste0(
        gsub('[B-DF-HJ-NP-TV-Z]', '', {{ forename }}),
        substr({{ surname }}, 1, 3),
        substr({{ postcode }}, 1, 3)
      )
    )
}
