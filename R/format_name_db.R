#' Formatting either a Forename or Surname within the DB
#'
#' Format the name strings prior to matching
#' Formatting includes conversion to upper case and removal of
#' non alphabetic characters
#'
#' @param df A df to be formatted
#' @param name_col a patient name column
#'
#' @return A df with cleansed patient name information
#'
#' @export
#'
#' @examples
#' format_db_name(df, name_col)
format_name_db <- function(df, name_col) {
  df %>%
    dplyr::mutate(
      # Remove non-alpha chars and convert emtpy string to NA
      {{ name_col }} := toupper(REGEXP_REPLACE({{ name_col }}, "[^[:alpha:]]", "")),
      {{ name_col }} := ifelse(nchar({{ name_col }}) == 0, NA, {{ name_col }})
    )
}
