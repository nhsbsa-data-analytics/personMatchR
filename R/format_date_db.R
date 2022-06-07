#' Format a DOB within the DB
#'
#' Format the date of birth prior to matching
#' Convert to a string representation in the format YYYYMMDD
#'
#' @param df A df to be formatted
#' @param name_col a date column
#'
#' @return A df with cleansed date information, as a 8-digit number
#'
#' @export
#'
#' @examples
#' format_db_date(df, date_col)
format_date_db <- function(df, date_col) {
  df %>%
    dplyr::mutate(
      {{ date_col }} := ifelse(
        REGEXP_INSTR({{ date_col }}, "[0-9]{8}$") == 1,
        TO_NUMBER({{ date_col }}),
        TO_NUMBER(TO_CHAR({{ date_col }}, "YYYYMMDD"))
      )
    )
}
