#' Format the date of birth prior to matching
#'
#' Formatting converts date field to numeric format using format YYYYMMDD
#' \cr\cr Date should be based on a recognised date type
#'
#' @param df a 'lazyframe' generated from a database connection with date field to be formatted
#' @param date_col a date column to be formatted
#'
#' @return a 'lazyframe' object with cleansed date of birth field
#'
#' @export
#'
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
