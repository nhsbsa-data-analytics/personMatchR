#' FormatName
#' Format the name strings prior to matching
#' Formatting includes conversion to upper case and removal of non alphabetic characters
#'
#' @param strName A string field to be cleansed
#'
#' @return A cleansed string
#' @export
#'
#' @examples
#' FormatName(strName)
FormatName <- function(strName) {

  #handle missing variables
  if(is.na(strName)) {return(strName)}

  # remove non alphanumeric characters and convert to upper case
  strNameFormat <- toupper(stringr::str_replace_all(strName, "[^[:alpha:]]", ""))

  # return formatted string
  return(strNameFormat)
}
