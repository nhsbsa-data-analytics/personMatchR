#' Format the postcode strings prior to matching
#'
#' Formatting includes conversion to upper case and removal of
#' non alphanumeric characters
#'
#' @param df A 'lazyframe' generated from a database connection with postcode to be formatted
#' @param postcode the postcode column to be formatted
#'
#' @return A cleansed string
#' @export
#'
#' @examples
#' format_postcode_db(df, postcode)
format_postcode_db <- function(df, postcode) {

  # Simple formatting of postcode
  df <- df %>%
    dplyr::mutate(POSTCODE_OLD := {{ postcode }})

  # Just Process distinct postcodes
  output <- df %>%
    dplyr::select(POSTCODE_OLD, {{ postcode }}) %>%
    dplyr::filter(!is.na({{ postcode }})) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      # Format and split postcode
      {{ postcode }} := ifelse(nchar({{ postcode }}) == 0, NA, {{ postcode }}),
      {{ postcode }} := toupper(REGEXP_REPLACE({{ postcode }}, "[^[:alnum:]]", "")),
      # Length vars to aid below logic
      LEN = nchar({{ postcode }}),

      # Long case-statement to correct postcode errors
      {{ postcode }} := dplyr::case_when(

        # 7 Character postcodes
        # Postcode Length 7 - # 1st Character
        LEN == 7 & substr({{ postcode }}, 1, 1) == "5" ~ replace({{ postcode }}, substr({{ postcode }}, 1, 1), "S"),
        LEN == 7 & substr({{ postcode }}, 1, 1) == "0" ~ replace({{ postcode }}, substr({{ postcode }}, 1, 1), "O"),
        # Postcode Length 7 - 2nd Character
        LEN == 7 & substr({{ postcode }}, 2, 2) == "5" ~ replace({{ postcode }}, substr({{ postcode }}, 2, 2), "S"),
        LEN == 7 & substr({{ postcode }}, 2, 2) == "0" ~ replace({{ postcode }}, substr({{ postcode }}, 2, 2), "O"),
        # Postcode Length 7 - 3nd Character
        LEN == 7 & substr({{ postcode }}, 3, 3) == "O" ~ replace({{ postcode }}, substr({{ postcode }}, 3, 3), "0"),
        LEN == 7 & substr({{ postcode }}, 3, 3) == "I" ~ replace({{ postcode }}, substr({{ postcode }}, 3, 3), "1"),
        LEN == 7 & substr({{ postcode }}, 3, 3) == "L" ~ replace({{ postcode }}, substr({{ postcode }}, 3, 3), "1"),
        LEN == 7 & substr({{ postcode }}, 3, 3) == "S" ~ replace({{ postcode }}, substr({{ postcode }}, 3, 3), "5"),
        # Postcode Length 7 - 5th Character
        LEN == 7 & substr({{ postcode }}, 5, 5) == "O" ~ replace({{ postcode }}, substr({{ postcode }}, 5, 5), "0"),
        LEN == 7 & substr({{ postcode }}, 5, 5) == "I" ~ replace({{ postcode }}, substr({{ postcode }}, 5, 5), "1"),
        LEN == 7 & substr({{ postcode }}, 5, 5) == "L" ~ replace({{ postcode }}, substr({{ postcode }}, 5, 5), "1"),
        LEN == 7 & substr({{ postcode }}, 5, 5) == "S" ~ replace({{ postcode }}, substr({{ postcode }}, 5, 5), "5"),
        # Postcode Length 7 - 6th Character
        LEN == 7 & substr({{ postcode }}, 6, 6) == "5" ~ replace({{ postcode }}, substr({{ postcode }}, 6, 6), "S"),
        LEN == 7 & substr({{ postcode }}, 6, 6) == "0" ~ replace({{ postcode }}, substr({{ postcode }}, 6, 6), "O"),
        # Postcode Length 7 - 7th Character
        LEN == 7 & substr({{ postcode }}, 7, 7) == "5" ~ replace({{ postcode }}, substr({{ postcode }}, 7, 7), "S"),
        LEN == 7 & substr({{ postcode }}, 7, 7) == "0" ~ replace({{ postcode }}, substr({{ postcode }}, 7, 7), "O"),

        # 6 Character postcodes
        # Postcode Length 6 - # 1st Character
        LEN == 6 & substr({{ postcode }}, 1, 1) == "5" ~ replace({{ postcode }}, substr({{ postcode }}, 1, 1), "S"),
        LEN == 6 & substr({{ postcode }}, 1, 1) == "0" ~ replace({{ postcode }}, substr({{ postcode }}, 1, 1), "O"),
        # Postcode Length 7 - 3nd Character
        LEN == 6 & substr({{ postcode }}, 4, 4) == "O" ~ replace({{ postcode }}, substr({{ postcode }}, 4, 4), "0"),
        LEN == 6 & substr({{ postcode }}, 4, 4) == "I" ~ replace({{ postcode }}, substr({{ postcode }}, 4, 4), "1"),
        LEN == 6 & substr({{ postcode }}, 4, 4) == "L" ~ replace({{ postcode }}, substr({{ postcode }}, 4, 4), "1"),
        LEN == 6 & substr({{ postcode }}, 4, 4) == "S" ~ replace({{ postcode }}, substr({{ postcode }}, 4, 4), "5"),
        # Postcode Length 7 - 2nd Character
        LEN == 6 & substr({{ postcode }}, 5, 5) == "5" ~ replace({{ postcode }}, substr({{ postcode }}, 5, 5), "S"),
        LEN == 6 & substr({{ postcode }}, 5, 5) == "0" ~ replace({{ postcode }}, substr({{ postcode }}, 5, 5), "O"),
        # Postcode Length 7 - 2nd Character
        LEN == 6 & substr({{ postcode }}, 6, 6) == "5" ~ replace({{ postcode }}, substr({{ postcode }}, 6, 6), "S"),
        LEN == 6 & substr({{ postcode }}, 6, 6) == "0" ~ replace({{ postcode }}, substr({{ postcode }}, 6, 6), "O"),

        # 5 Character postcodes
        # Postcode Length 7 - # 1st Character
        LEN == 5 & substr({{ postcode }}, 1, 1) == "5" ~ replace({{ postcode }}, substr({{ postcode }}, 1, 1), "S"),
        LEN == 5 & substr({{ postcode }}, 1, 1) == "0" ~ replace({{ postcode }}, substr({{ postcode }}, 1, 1), "O"),
        # Postcode Length 7 - 5th Character
        LEN == 5 & substr({{ postcode }}, 2, 2) == "O" ~ replace({{ postcode }}, substr({{ postcode }}, 2, 2), "0"),
        LEN == 5 & substr({{ postcode }}, 2, 2) == "I" ~ replace({{ postcode }}, substr({{ postcode }}, 2, 2), "1"),
        LEN == 5 & substr({{ postcode }}, 2, 2) == "L" ~ replace({{ postcode }}, substr({{ postcode }}, 2, 2), "1"),
        LEN == 5 & substr({{ postcode }}, 2, 2) == "S" ~ replace({{ postcode }}, substr({{ postcode }}, 2, 2), "5"),
        # Postcode Length 7 - 3nd Character
        LEN == 5 & substr({{ postcode }}, 3, 3) == "O" ~ replace({{ postcode }}, substr({{ postcode }}, 3, 3), "0"),
        LEN == 5 & substr({{ postcode }}, 3, 3) == "I" ~ replace({{ postcode }}, substr({{ postcode }}, 3, 3), "1"),
        LEN == 5 & substr({{ postcode }}, 3, 3) == "L" ~ replace({{ postcode }}, substr({{ postcode }}, 3, 3), "1"),
        LEN == 5 & substr({{ postcode }}, 3, 3) == "S" ~ replace({{ postcode }}, substr({{ postcode }}, 3, 3), "5"),
        # Postcode Length 7 - 6th Character
        LEN == 5 & substr({{ postcode }}, 4, 4) == "5" ~ replace({{ postcode }}, substr({{ postcode }}, 4, 4), "S"),
        LEN == 5 & substr({{ postcode }}, 4, 4) == "0" ~ replace({{ postcode }}, substr({{ postcode }}, 4, 4), "O"),
        # Postcode Length 7 - 2nd Character
        LEN == 5 & substr({{ postcode }}, 5, 5) == "5" ~ replace({{ postcode }}, substr({{ postcode }}, 5, 5), "S"),
        LEN == 5 & substr({{ postcode }}, 5, 5) == "0" ~ replace({{ postcode }}, substr({{ postcode }}, 5, 5), "O"),

        # Remaining postcodes
        T ~ {{ postcode }}
      )
    )

  # Rejoin back to original data
  df <- df %>%
    dplyr::select(-{{ postcode }}) %>%
    dplyr::left_join(y = output, by = "POSTCODE_OLD") %>%
    dplyr::select(-c(LEN, POSTCODE_OLD))

  # Return formatted df
  return(df)
}
