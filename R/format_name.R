#' Formatting either a Forename or Surname
#'
#' Format the name strings prior to matching
#' Formatting includes conversion to upper case and removal of
#' non alphabetic characters
#'
#' @param str_name A string field to be cleansed
#'
#' @return A cleansed string
#'
#' @export
#'
#' @examples
#' format_name(str_name)
format_name <- function(str_name) {

  # handle missing variables
  if (is.na(str_name) || is.null(str_name) || str_name == "") {
    return(NA)
  }

  # convert string to uppercase remove non alphabetic characters
  return(toupper(gsub("[^[:alpha:]]", "", str_name)))
}
