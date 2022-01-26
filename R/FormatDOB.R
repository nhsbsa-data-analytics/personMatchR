#' Format a DOB
#'
#' Format the date of birth prior to matching
#' Convert to a string representation in the format YYYYMMDD
#'
#' @param dob A date of birth date field to be cleansed
#'
#' @return A cleansed string
#'
#' @export
#'
#' @examples
#' format_dob(dob)
format_dob <- function(dob){

  # Convert dob to character if it isn't NA
  ifelse(is.na(dob), dob, format(as.Date(dob), '%Y%m%d'))
}

#format_dob <- function(x) ifelse(is.na(x), x, format(as.Date(x), '%Y%m%d'))
