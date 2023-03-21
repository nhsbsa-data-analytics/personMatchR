#' Format name strings prior to matching
#'
#' Suitable for formatting forename/surname strings prior to matching
#' \cr\cr Formatting includes conversion to upper case and removal of non-alphabetic characters
#'
#' @param df a 'lazyframe' generated from a database connection with postcode to be formatted
#' @param name_col a string field to be cleansed
#'
#' @return a 'lazyframe' object with cleansed name string data
#'
#' @export
#'
format_name_db <- function(df, name_col) {
  df %>%
    dplyr::mutate(
      # Remove non-alpha chars and convert emtpy string to NA
      {{ name_col }} := toupper(REGEXP_REPLACE({{ name_col }}, "[^[:alpha:]]", "")),
      {{ name_col }} := ifelse(nchar({{ name_col }}) == 0, NA, {{ name_col }})
    )
}
