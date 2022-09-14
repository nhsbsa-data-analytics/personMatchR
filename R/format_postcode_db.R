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
      # copy the postcode
      PCD_TEMP = {{ postcode }},
      # each potential transposition needs to be handled as a separate if statement
      # 7 character postcodes : 1st character (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 1, 1) == "5", paste0("S", substr(PCD_TEMP, 2, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 1, 1) == "0", paste0("O", substr(PCD_TEMP, 2, 7)), PCD_TEMP),
      # 7 character postcodes : 2nd character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 2, 2) == "5", paste0(substr(PCD_TEMP, 1, 1), "S", substr(PCD_TEMP, 3, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 2, 2) == "0", paste0(substr(PCD_TEMP, 1, 1), "O", substr(PCD_TEMP, 3, 7)), PCD_TEMP),
      # 7 character postcodes : 3rd character  (should be number)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 3, 3) == "S", paste0(substr(PCD_TEMP, 1, 2), "5", substr(PCD_TEMP, 4, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 3, 3) == "O", paste0(substr(PCD_TEMP, 1, 2), "0", substr(PCD_TEMP, 4, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 3, 3) == "I", paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 3, 3) == "L", paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 7)), PCD_TEMP),
      # 7 character postcodes : 5th character  (should be number)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 5, 5) == "S", paste0(substr(PCD_TEMP, 1, 4), "5", substr(PCD_TEMP, 6, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 5, 5) == "O", paste0(substr(PCD_TEMP, 1, 4), "0", substr(PCD_TEMP, 6, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 5, 5) == "I", paste0(substr(PCD_TEMP, 1, 4), "1", substr(PCD_TEMP, 6, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 5, 5) == "L", paste0(substr(PCD_TEMP, 1, 4), "1", substr(PCD_TEMP, 6, 7)), PCD_TEMP),
      # 7 character postcodes : 6th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 6, 6) == "5", paste0(substr(PCD_TEMP, 1, 5), "S", substr(PCD_TEMP, 7, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 6, 6) == "0", paste0(substr(PCD_TEMP, 1, 5), "O", substr(PCD_TEMP, 7, 7)), PCD_TEMP),
      # 7 character postcodes : 7th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 7, 7) == "5", paste0(substr(PCD_TEMP, 1, 6), "S"), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 7, 7) == "0", paste0(substr(PCD_TEMP, 1, 6), "O"), PCD_TEMP),
      # 6 character postcodes : 1st character (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 1, 1) == "5", paste0("S", substr(PCD_TEMP, 2, 6)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 1, 1) == "0", paste0("O", substr(PCD_TEMP, 2, 6)), PCD_TEMP),
      # 6 character postcodes : 4th character  (should be number)
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 4, 4) == "S", paste0(substr(PCD_TEMP, 1, 3), "5", substr(PCD_TEMP, 5, 6)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 4, 4) == "O", paste0(substr(PCD_TEMP, 1, 3), "0", substr(PCD_TEMP, 5, 6)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 4, 4) == "I", paste0(substr(PCD_TEMP, 1, 3), "1", substr(PCD_TEMP, 5, 6)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 4, 4) == "L", paste0(substr(PCD_TEMP, 1, 3), "1", substr(PCD_TEMP, 5, 6)), PCD_TEMP),
      # 6 character postcodes : 5th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 5, 5) == "5", paste0(substr(PCD_TEMP, 1, 4), "S", substr(PCD_TEMP, 6, 6)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 5, 5) == "0", paste0(substr(PCD_TEMP, 1, 4), "O", substr(PCD_TEMP, 6, 6)), PCD_TEMP),
      # 6 character postcodes : 6th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 6, 6) == "5", paste0(substr(PCD_TEMP, 1, 5), "S"), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 6, 6) == "0", paste0(substr(PCD_TEMP, 1, 5), "O"), PCD_TEMP),
      # 5 character postcodes : 1st character (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 1, 1) == "5", paste0("S", substr(PCD_TEMP, 2, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 1, 1) == "0", paste0("O", substr(PCD_TEMP, 2, 5)), PCD_TEMP),
      # 5 character postcodes : 2nd character  (should be number)
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 2, 2) == "S", paste0(substr(PCD_TEMP, 1, 1), "5", substr(PCD_TEMP, 3, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 2, 2) == "O", paste0(substr(PCD_TEMP, 1, 1), "0", substr(PCD_TEMP, 3, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 2, 2) == "I", paste0(substr(PCD_TEMP, 1, 1), "1", substr(PCD_TEMP, 3, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 2, 2) == "L", paste0(substr(PCD_TEMP, 1, 1), "1", substr(PCD_TEMP, 3, 5)), PCD_TEMP),
      # 5 character postcodes : 3rd character  (should be number)
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 3, 3) == "S", paste0(substr(PCD_TEMP, 1, 2), "5", substr(PCD_TEMP, 4, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 3, 3) == "O", paste0(substr(PCD_TEMP, 1, 2), "0", substr(PCD_TEMP, 4, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 3, 3) == "I", paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 3, 3) == "L", paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 5)), PCD_TEMP),
      # 5 character postcodes : 4th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 4, 4) == "5", paste0(substr(PCD_TEMP, 1, 3), "S", substr(PCD_TEMP, 5, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 4, 4) == "0", paste0(substr(PCD_TEMP, 1, 3), "O", substr(PCD_TEMP, 5, 5)), PCD_TEMP),
      # 5 character postcodes : 6th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 5, 5) == "5", paste0(substr(PCD_TEMP, 1, 4), "S"), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 5, 5) == "0", paste0(substr(PCD_TEMP, 1, 4), "O"), PCD_TEMP),
      # replace postcode with formatted string
      {{ postcode }} := PCD_TEMP
    )


  # Rejoin back to original data
  df <- df %>%
    dplyr::select(-{{ postcode }}) %>%
    dplyr::left_join(y = output, by = "POSTCODE_OLD") %>%
    dplyr::select(-c(LEN, POSTCODE_OLD, PCD_TEMP))

  # Return formatted df
  return(df)
}
