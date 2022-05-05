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
