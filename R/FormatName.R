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

  # Remove non-alphanumeric characters and convert to upper if name isn't NA
  ifelse(is.na(str_name), str_name, toupper(gsub("[^[:alpha:]]", "", str_name)))
}
