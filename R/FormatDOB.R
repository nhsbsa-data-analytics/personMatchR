#' FormatDOB
#' Format the date of birth prior to matching
#' Convert to a string representation in the format YYYYMMDD
#'
#' @param dob A field to be cleansed
#'
#' @return A cleansed string
#' @export
#'
#' @examples
#' FormatDOB(dob)
FormatDOB <- function(dob) {

  # convert to character string in format YYYYMMDD
  strDOBFormat <- format(as.Date(dob),'%Y%m%d')

  # return formatted string
  return(strDOBFormat)
}
